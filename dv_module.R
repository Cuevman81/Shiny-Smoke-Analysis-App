# dv_module.R — EE Design Value module
# Ported from EE_DV_Recalculation_with_AQSR.R for embedding in the Smoke Analysis app.
# Engine (AQS fetch, EPA Appendix N design value, candidate-day finder) is verbatim;
# server is wrapped as a Shiny module (dvServer); UI is provided as bslib (dvUI).
# Requires: RAQSAPI credentials set by the host app (aqs_credentials()).

hms_cache_dir <- "hms_smoke_data"
if (!dir.exists(hms_cache_dir)) dir.create(hms_cache_dir, showWarnings = FALSE)

# ============================================================
# Engine / helper functions
# ============================================================
get_hms_smoke <- function(date_val) {
  date_str <- format(as.Date(date_val), "%Y%m%d")
  year_str <- format(as.Date(date_val), "%Y")
  month_str <- format(as.Date(date_val), "%m")
  
  zip_name <- paste0("hms_smoke", date_str, ".zip")
  zip_path <- file.path(hms_cache_dir, zip_name)
  shape_dir <- file.path(hms_cache_dir, date_str)
  
  # NOAA URL Structure: .../Shapefile/YYYY/MM/hms_smokeYYYYMMDD.zip
  url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/", 
                year_str, "/", month_str, "/", zip_name)
  
  if (!dir.exists(shape_dir)) {
    if (!file.exists(zip_path)) {
      message("Downloading HMS smoke for ", date_val, "...")
      res <- try(download.file(url, zip_path, mode = "wb", quiet = TRUE), silent = TRUE)
      if (inherits(res, "try-error") || res != 0) {
        message("Could not download HMS smoke for ", date_val)
        return(NULL)
      }
    }
    
    dir.create(shape_dir)
    unzip(zip_path, exdir = shape_dir)
  }
  
  # Find the .shp file
  shp_file <- list.files(shape_dir, pattern = "\\.shp$", full.names = TRUE)
  if (length(shp_file) == 0) return(NULL)
  
  tryCatch({
    smoke <- sf::st_read(shp_file[1], quiet = TRUE)
    # Ensure CRS is WGS84 (HMS shapefiles are WGS84 but the .prj is sometimes missing)
    if (is.na(sf::st_crs(smoke))) smoke <- sf::st_set_crs(smoke, 4326)
    else if (!isTRUE(sf::st_crs(smoke)$epsg == 4326)) smoke <- sf::st_transform(smoke, 4326)
    # Fix invalid geometries (common in HMS shapefiles)
    smoke <- sf::st_make_valid(smoke)
    # Normalize HMS density labels to Title Case (Light / Medium / Heavy) so this
    # one cached function can serve both the EE DV module and the smoke tabs.
    if ("Density" %in% names(smoke)) smoke$Density <- stringr::str_to_title(as.character(smoke$Density))
    else if ("DENSITY" %in% names(smoke)) smoke$Density <- stringr::str_to_title(as.character(smoke$DENSITY))
    return(smoke)
  }, error = function(e) {
    message("Error reading HMS shapefile: ", e$message)
    return(NULL)
  })
}

# Helper function to check smoke impact at a site
check_smoke_impact <- function(smoke_sf, lat, lon) {
  if (is.null(smoke_sf)) return("No Data")
  
  site_point <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  
  # Spatial intersection
  suppressMessages({
    intersection <- sf::st_intersection(smoke_sf, site_point)
  })
  
  if (nrow(intersection) > 0) {
    if ("Density" %in% colnames(intersection)) {
      # Prioritize highest density (Heavy > Medium > Light)
      intersection$density_rank <- case_when(
        as.character(intersection$Density) == "Heavy" ~ 3,
        as.character(intersection$Density) == "Medium" ~ 2,
        as.character(intersection$Density) == "Light" ~ 1,
        TRUE ~ 0
      )
      
      # Sort by rank descending and take the top one
      top_impact <- intersection %>%
        dplyr::arrange(desc(density_rank)) %>%
        dplyr::slice(1)
      
      return(as.character(top_impact$Density))
    } else {
      return("In Plume")
    }
  }
  
  return("None")
}

fetch_and_process_epa_data <- function(start_year, end_year, state, county, site, selected_method_code) { 
  # --- Cache Check ---
  # Create a unique filename for the cache based on site, start year, and end year
  # We'll need the site name for a better filename, but site number is safer for uniqueness
  cache_name <- paste0("site_", state, "_", county, "_", site, "_DV", end_year, ".rds")
  cache_path <- file.path("aqs_data_cache", cache_name)
  
  if (file.exists(cache_path)) {
    message("Loading AQS data from cache: ", cache_name)
    return(readRDS(cache_path))
  }
  
  message("Fetching fresh AQS data from EPA API...")
  data_list <- list()
  
  # Loop through each year needed for the design value
  for (year in start_year:end_year) {
    bdate <- as.Date(paste0(year, "-01-01"))
    edate <- as.Date(paste0(year, "-12-31"))
    
    # Fetch data for the year
    year_data <- tryCatch({
      aqs_dailysummary_by_state(
        parameter = "88101",
        bdate = bdate,
        edate = edate,
        stateFIPS = state
      )
    }, error = function(e) {
      message(paste("Network Error fetching data for year", year, ":", e$message))
      return(NULL)
    })
    
    # Process if data was fetched
    if (!is.null(year_data) && nrow(year_data) > 0) {
      
      # --- Pre-Filter Debug ---
      site_raw_data <- year_data[year_data$county_code == county & year_data$site_number == site, ]
      if (nrow(site_raw_data) > 0) {
        print(paste("--- Debug Info for Site", county, "-", site, "in Year", year, "(Pre-Filter) ---"))
        print(paste("Raw records for site:", nrow(site_raw_data)))
        print("Unique Method Codes found at site for this year:")
        print(unique(site_raw_data$method_code))
        print("Unique Sample Duration Codes found at site for this year:")
        print(unique(site_raw_data$sample_duration_code)) # IMPORTANT CHECK
        print("-------------------------------------------------")
      } else {
        print(paste("--- No raw data found FOR SITE", county, "-", site, "in Year", year, "---"))
      }
      # --- End Pre-Filter Debug ---
      
      
      # --- Determine Relevant Method Code(s) for THIS Specific Year ---
      method_to_use_this_year <- character(0) # Initialize empty vector for codes
      selected_code_from_ui <- as.character(selected_method_code)
      
      # Logic for the T640 family (handles 736 <-> 636 transition)
      if (selected_code_from_ui == "T640_TRANSITION") {
        if (year < 2024) {
          method_to_use_this_year <- "736"
        } else { # For 2024 and beyond
          method_to_use_this_year <- "636"
        }
        # Logic for the T640x family (handles 738 <-> 638 transition)
      } else if (selected_code_from_ui == "T640X_TRANSITION") {
        if (year < 2024) {
          method_to_use_this_year <- "738"
        } else { # For 2024 and beyond
          method_to_use_this_year <- "638"
        }
        # Fallback for all other non-transitioning codes (e.g., "145", "188")
      } else {
        method_to_use_this_year <- selected_code_from_ui
      }
      
      # Check if we found a relevant code for this year
      if (length(method_to_use_this_year) == 0 || method_to_use_this_year == "") {
        print(paste("WARN: No applicable method code determined for year", year, "based on selected code:", selected_method_code, ". Skipping year."))
        filtered_data <- year_data[0,] # Create an empty data frame structure
      } else {
        print(paste("Filtering year", year, "using method code:", method_to_use_this_year)) # Should be only one code now
        
        # --- Apply Filter using Year-Specific Code AND DAILY Duration ---
        county_char <- as.character(county)
        site_char <- as.character(site)
        param_code_char <- "88101"
        # *** FIX: Filter for DAILY durations only ***
        duration_codes_char <- c("1", "X") # Use '1' (24-HR) and 'X' (DAILY)
        
        filtered_data <- year_data %>%
          dplyr::filter(
            as.character(county_code) == county_char,
            as.character(site_number) == site_char,
            as.character(parameter_code) == param_code_char,
            as.character(method_code) %in% method_to_use_this_year, # Use the single determined code
            as.character(sample_duration_code) %in% duration_codes_char # Filter for daily duration
          )
        # --- End Apply Filter ---
      }
      
      # Add the filtered data (might be empty) to the list
      data_list[[as.character(year)]] <- filtered_data
      
      # --- Post-Filter Debug ---
      print(paste("Year:", year))
      print(paste("Raw records fetched (State Total):", nrow(year_data)))
      print(paste("Records after filtering (county, site, param, METHOD, DURATION):", nrow(filtered_data))) # Updated description
      print(paste("Unique Method Codes Found (in filtered_data):", paste(unique(filtered_data$method_code), collapse=", ")))
      print(paste("Unique Duration Codes Found (in filtered_data):", paste(unique(filtered_data$sample_duration_code), collapse=", "))) # Added check
      print(paste("Records added to list for year", year, "(after filtering):", nrow(filtered_data)))
      
    } else {
      print(paste("Year:", year, " - No raw data fetched for state/year or error occurred during fetch."))
    }
  } # End of for loop
  
  # --- Combine and Process Data ---
  if (length(data_list) == 0) {
    stop("No data added to list across specified years; check filter conditions and raw data availability at the site.")
  }
  combined_data <- dplyr::bind_rows(data_list)
  if (nrow(combined_data) == 0) {
    stop("No data found matching all specified criteria (Site, Parameter, Method, Duration) after combining years.")
  }
  
  # Select the official daily average ('arithmetic_mean')
  # Ensure distinct dates - if multiple POCs/methods provide a DAILY value, pick one (lowest POC is common)
  processed_data <- combined_data %>%
    transmute(
      state_code = as.character(state_code),
      county_code = as.character(county_code),
      site_number = as.character(site_number),
      poc = as.integer(poc),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      sample_duration = as.character(sample_duration), # Keep original duration description
      date = as.Date(date_local),
      pm25 = as.numeric(arithmetic_mean), # This is the daily mean from aqs_dailyData
      method_code = as.character(method_code),
      method_name = as.character(method), # Use the name from AQS
      local_site_name = as.character(local_site_name),
      county = as.character(county),
      city = as.character(city),
      year = as.numeric(format(date, "%Y")),
      excluded = FALSE # Initialize excluded flag
    ) %>%
    # Handle potential multiple daily records for the same site/day (e.g., different POCs)
    # Keep the record with the lowest POC for each day
    dplyr::arrange(date, poc) %>%
    dplyr::distinct(date, .keep_all = TRUE) %>% # Keeps the first row for each date (lowest POC due to arrange)
    dplyr::arrange(date) # Sort final data by date
  
  # --- Save to Cache ---
  if (!is.null(processed_data) && nrow(processed_data) > 0) {
    if (!dir.exists("aqs_data_cache")) dir.create("aqs_data_cache")
    message("Saving AQS data to cache: ", cache_name)
    saveRDS(processed_data, cache_path)
  }

  if (nrow(processed_data) == 0) {
    stop("Data processing resulted in an empty dataset. Check distinct() logic or upstream filters.")
  }
  
  print(paste("Final processed data rows (should be ~365 per year):", nrow(processed_data)))
  print("Structure of final processed_data before returning:")
  str(processed_data)
  
  return(processed_data)
}

# Calendar days in a given quarter (Q1 gains a day in leap years).
# Used as the denominator for per-quarter completeness (continuous monitors).
days_in_quarter <- function(year, q) {
  leap <- (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
  base <- c(90, 91, 92, 92)[q]            # Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec
  base + ifelse(q == 1 & leap, 1, 0)
}

# Modified function to calculate design value
# EPA Appendix N (40 CFR 50): the annual mean is the average of the four
# QUARTERLY means (not a straight mean of daily values); the design value is the
# 3-year average of the annual means, rounded to the nearest 0.1 ug/m3. Per-quarter
# completeness (>=75%) governs validity. This matches the AQS Design Value Report.
calculate_design_value <- function(data, dv_year) {
  print(paste("--- Calculating Design Value for DV Year:", dv_year, "(Using Working App Logic) ---"))
  
  tryCatch({
    # Ensure we have data
    if (is.null(data) || nrow(data) == 0) {
      print("Error Condition: Input data is NULL or empty.")
      return(list(
        annual_means = data.frame(), design_value = NA_real_,
        has_method_transition = FALSE, error = "No data available for calculation"
      ))
    }
    # --- Add check for input data structure ---
    print("Structure of INPUT data to calculate_design_value:")
    str(data)
    required_cols_input <- c("year", "excluded", "pm25", "method_code", "date")
    if (!all(required_cols_input %in% names(data))) {
      missing_cols <- setdiff(required_cols_input, names(data))
      print(paste("Error Condition: Input data missing required columns:", paste(missing_cols, collapse=", ")))
      return(list( annual_means = data.frame(), design_value = NA_real_, has_method_transition = FALSE, error = paste("Input data missing required columns:", paste(missing_cols, collapse=", "))))
    }
    # --- End check ---
    
    
    # Filter data for the DV period and non-excluded points
    dv_period_data <- data %>%
      dplyr::filter(!excluded) %>%
      dplyr::filter(year >= (dv_year - 2), year <= dv_year)
    
    print(paste("Rows in dv_period_data (after filtering):", nrow(dv_period_data)))
    if(nrow(dv_period_data) == 0) {
      print("Error Condition: No non-excluded data for DV years.")
      return(list(
        annual_means = data.frame(), design_value = NA_real_,
        has_method_transition = FALSE, error = paste("No non-excluded data available for years", dv_year-2, "to", dv_year)
      ))
    }
    print("Structure of dv_period_data BEFORE summarize:")
    str(dv_period_data)
    print(paste("Is dv_period_data$year numeric?", is.numeric(dv_period_data$year)))
    
    
    # --- EPA Appendix N annual mean = average of the four QUARTERLY means ---
    # Step 1: quarterly means (full precision) + per-quarter completeness.
    quarterly_means <- dv_period_data %>%
      dplyr::mutate(quarter = lubridate::quarter(date)) %>%
      dplyr::group_by(year, quarter) %>%
      dplyr::summarize(
        quarter_mean = mean(pm25, na.rm = TRUE),
        q_obs        = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        days_in_q      = days_in_quarter(year, quarter),
        q_completeness = (q_obs / days_in_q) * 100
      )

    # Step 2: annual mean = mean of the (up to 4) quarterly means, rounded to 1 dp
    #         (AQS convention). Annual validity requires all 4 quarters >= 75%.
    annual_means <- quarterly_means %>%
      dplyr::group_by(year) %>%
      dplyr::summarize(
        annual_mean        = round(mean(quarter_mean, na.rm = TRUE), 1),
        n_quarters         = dplyr::n(),
        n_observations     = sum(q_obs),
        data_completeness  = mean(q_completeness, na.rm = TRUE),   # annual figure for display
        min_q_completeness = min(q_completeness, na.rm = TRUE),    # EPA validity is per-quarter
        all_quarters_valid = (dplyr::n() == 4) && all(q_completeness >= 75),
        is_leap            = (dplyr::first(year) %% 4 == 0 & dplyr::first(year) %% 100 != 0) | (dplyr::first(year) %% 400 == 0),
        days_in_year       = ifelse(is_leap, 366, 365),
        .groups = "drop"
      ) %>%
      # Attach the per-year method codes (for transition detection downstream)
      dplyr::left_join(
        dv_period_data %>%
          dplyr::group_by(year) %>%
          dplyr::summarize(
            method_codes = paste(sort(unique(as.character(method_code))), collapse = ", "),
            .groups = "drop"
          ),
        by = "year"
      ) %>%
      dplyr::select(year, annual_mean, data_completeness, n_observations, days_in_year,
                    method_codes, n_quarters, min_q_completeness, all_quarters_valid)
    
    
    print("Calculated annual_means table:")
    print(as.data.frame(annual_means))
    
    
    # Check if we have data for all 3 required years
    required_years <- (dv_year - 2):dv_year
    if (!all(required_years %in% annual_means$year)) {
      missing_years <- setdiff(required_years, annual_means$year)
      print(paste("Error Condition: Missing annual means for year(s):", paste(missing_years, collapse=", ")))
      # Return partial means if available, but flag error
      return(list(
        annual_means = annual_means, design_value = NA_real_,
        has_method_transition = FALSE, # Cannot determine transition reliably
        error = paste("Insufficient data: Missing annual mean(s) for year(s):", paste(missing_years, collapse=", "))
      ))
    }
    
    # Design value = 3-year average of the (rounded) annual means, rounded to
    # the nearest 0.1 ug/m3 per Appendix N. This reproduces the AQS DVR value.
    design_value <- round(mean(annual_means$annual_mean, na.rm = TRUE), 1)
    print(paste("Calculated Design Value:", design_value))
    
    # Check if design value is valid
    if (is.na(design_value) || !is.numeric(design_value)) {
      print("Error Condition: Invalid design value calculated (NA or non-numeric).")
      has_transition_check <- tryCatch({ length(unique(unlist(strsplit(annual_means$method_codes, ",\\s*")))) > 1 }, error = function(e) { FALSE })
      return(list(
        annual_means = annual_means, design_value = NA_real_,
        has_method_transition = has_transition_check, error = "Invalid design value calculated (possibly due to NA annual means)"
      ))
    }
    
    # Check for method code transition warning
    all_methods_in_period <- tryCatch({ unique(unlist(strsplit(annual_means$method_codes, ",\\s*"))) }, error = function(e) { character(0) })
    has_transition <- length(all_methods_in_period) > 1
    print(paste("Method Transition detected:", has_transition))
    
    print("--- Design Value Calculation Successful ---")
    return(list(
      annual_means = annual_means,
      design_value = design_value,
      has_method_transition = has_transition,
      error = NULL
    ))
    
  }, error = function(e) {
    print(paste("!!! UNCAUGHT ERROR in calculate_design_value:", e$message))
    print("Call Stack (within error handler):")
    try(print(sys.calls()), silent=TRUE)
    return(list(
      annual_means = data.frame(), design_value = NA_real_,
      has_method_transition = FALSE, error = paste("Error during design value calculation:", e$message)
    ))
  })
}

# Calendar heatmap function
create_calendar_heatmap <- function(data, year, title) {
  # Ensure data is provided and not empty for the specific year
  if (is.null(data) || nrow(data) == 0) {
    # Return an empty plot or a message plot
    return(plotly::plot_ly() %>% layout(title = paste(title, "(No data available)")))
  }
  
  data_for_plot <- data %>%
    dplyr::filter(year == !!year)
  
  # Handle case where there's no data for the specific year after filtering
  if (nrow(data_for_plot) == 0) {
    return(plotly::plot_ly() %>% layout(title = paste(title, "(No data available for this year)")))
  }
  
  data_for_plot <- data_for_plot %>%
    dplyr::mutate(month = month(date),
           day = day(date),
           # This 'aqi_color' column is not used for the heatmap color, but can be kept for other purposes
           aqi_color = get_aqi_color(pm25),
           # Add marker for excluded dates
           marker_symbol = ifelse(excluded, "x", "circle"),
           marker_size = ifelse(excluded, 15, 8),
           # Enhanced hover text
           hover_text = paste("Date:", date,
                              "<br>PM2.5:", round(pm25, 1), "µg/m³",
                              "<br>AQI Category:", case_when(
                                pm25 <= 9.0 ~ "Good",
                                pm25 <= 35.4 ~ "Moderate",
                                pm25 <= 55.4 ~ "Unhealthy for Sensitive Groups",
                                pm25 <= 150.4 ~ "Unhealthy",
                                pm25 <= 250.4 ~ "Very Unhealthy",
                                TRUE ~ "Hazardous"
                              ),
                              "<br><b>Status:</b>", ifelse(excluded, 
                                                           "<span style='color:red'>EXCLUDED</span>", 
                                                           "Included")))
  
  # Define the fixed AQI color scale breakpoints and colors
  aqi_breaks <- c(0, 9.0, 35.4, 55.4, 150.4, 250.4, Inf)
  aqi_colors <- c("#00e400", "#ffff00", "#ff7e00", "#ff0000", "#8f3f97", "#7e0023")
  
  # Create the colorscale mapping for plotly
  # Normalize breaks to 0-1 range based on the max value in the data
  max_val_plot <- max(data_for_plot$pm25, na.rm = TRUE)
  # Ensure the scale at least covers the "Good" and "Moderate" ranges for consistency
  if (max_val_plot < 35.4) {
    max_val_plot <- 35.4
  }
  normalized_breaks <- pmin(aqi_breaks / max_val_plot, 1) # Normalize and cap at 1
  
  # Create the plotly colorscale list format for discrete color blocks
  colorscale <- list()
  for (i in 1:length(aqi_colors)) {
    # Lower bound of the color interval
    colorscale <- c(colorscale, list(c(normalized_breaks[i], aqi_colors[i])))
    # Upper bound of the color interval (using the same color)
    colorscale <- c(colorscale, list(c(normalized_breaks[i+1], aqi_colors[i])))
  }
  
  # Create the main plot
  p <- plot_ly(data_for_plot, x = ~day, y = ~month, z = ~pm25, type = "heatmap",
               coloraxis = "coloraxis",
               hoverinfo = "text",
               text = ~hover_text) %>%
    layout(title = title,
           xaxis = list(title = "Day of Month", dtick = 5),
           yaxis = list(title = "Month", tickvals = 1:12,
                        ticktext = month.abb, autorange = "reversed"),
           coloraxis = list(
             colorbar = list(title = "PM2.5 (µg/m³)",
                             tickvals = aqi_breaks[2:6], # Ticks at breakpoints
                             ticktext = as.character(aqi_breaks[2:6])),
             # --- FIX: Use the correctly formatted 'colorscale' object ---
             colorscale = colorscale,
             # -------------------------------------------------------------
             cmin = 0,
             cmax = max_val_plot
           )
    )
  
  # Add excluded points as a separate trace
  if (any(data_for_plot$excluded)) {
    p <- p %>% add_trace(
      data = dplyr::filter(data_for_plot, excluded),
      x = ~day, y = ~month,
      type = 'scatter',
      mode = 'markers',
      inherit = FALSE,
      marker = list(symbol = 'x', size = 15, color = 'black', line = list(width=3)), # Made marker more visible
      name = 'Excluded',
      hoverinfo = 'text',
      text = ~hover_text,
      showlegend = TRUE
    )
  }
  
  return(p)
}


# AQI Color function (make sure it matches create_calendar_heatmap logic)
get_aqi_color <- function(pm25) {
  case_when(
    is.na(pm25) ~ "#cccccc", # Grey for NA values
    pm25 <= 9.0 ~ "#00e400",  # Green - Good
    pm25 <= 35.4 ~ "#ffff00",  # Yellow - Moderate
    pm25 <= 55.4 ~ "#ff7e00",  # Orange - Unhealthy for Sensitive Groups
    pm25 <= 150.4 ~ "#ff0000", # Red - Unhealthy
    pm25 <= 250.4 ~ "#8f3f97", # Purple - Very Unhealthy
    TRUE ~ "#7e0023"           # Maroon - Hazardous
  )
}

get_state_info <- function() {
  data.frame(
    state_code = "28",
    state_name = "Mississippi",
    stringsAsFactors = FALSE
  )
}

# Function to get counties for a state
get_county_info <- function(state_code) {
  state_code <- as.character(state_code)
  message("RAQSAPI: Fetching counties for state FIPS: ", state_code)
  
  tryCatch({
    county_data <- aqs_counties_by_state(stateFIPS = state_code)
    
    if (!is.null(county_data) && nrow(county_data) > 0) {
      message("RAQSAPI: Received ", nrow(county_data), " counties.")
      message("RAQSAPI: Column names found: ", paste(colnames(county_data), collapse = ", "))
      
      # Use column position or any_of to be safe
      counties <- county_data %>%
        dplyr::mutate(
          county_code = sprintf("%03d", as.numeric(.data[[colnames(county_data)[1]]])),
          county_name = as.character(.data[[colnames(county_data)[2]]])
        ) %>%
        dplyr::select(county_code, county_name) %>% 
        dplyr::arrange(county_code) %>%
        dplyr::distinct() 
      return(counties)
    } else {
      message("RAQSAPI: API call returned NULL or empty for state: ", state_code)
      return(data.frame(county_code=character(), county_name=character())) 
    }
  }, error = function(e) {
    message("RAQSAPI Error (Counties): ", e$message)
    print(e)
    return(data.frame(county_code=character(), county_name=character()))
  })
}

# Function to get sites for a county
# Function to get sites for a county
get_site_info <- function(state_code, county_code) {
  state_code <- sprintf("%02d", as.numeric(state_code))
  county_code <- sprintf("%03d", as.numeric(county_code))
  message("RAQSAPI: Fetching active PM2.5 monitors for state: ", state_code, ", county: ", county_code)
  
  tryCatch({
    # Use monitors service to get status information (active vs inactive)
    # Note: RAQSAPI::aqs_monitors_by_county requires bdate and edate
    monitor_data <- aqs_monitors_by_county(
      stateFIPS = state_code, 
      countycode = county_code, 
      parameter = "88101",
      bdate = as.Date("2023-01-01"),
      edate = as.Date(format(Sys.Date(), "%Y-%m-%d"))
    )
    
    if (!is.null(monitor_data) && nrow(monitor_data) > 0) {
      message("RAQSAPI: Received ", nrow(monitor_data), " monitor records.")
      message("RAQSAPI: Column names found for monitors: ", paste(colnames(monitor_data), collapse = ", "))
      
      # Filter for active monitors (no end_date)
      # Common column names for end date: end_date, last_sample_date
      status_cols <- intersect(c("end_date", "last_sample_date"), colnames(monitor_data))
      
      if (length(status_cols) > 0) {
        status_col <- status_cols[1]
        message("RAQSAPI: Filtering for active monitors using column: ", status_col)
        # Keep only rows where status_col is NA (active)
        monitor_data <- monitor_data[is.na(monitor_data[[status_col]]), ]
        message("RAQSAPI: ", nrow(monitor_data), " active monitors remaining.")
      }
      
      if (nrow(monitor_data) == 0) {
        message("RAQSAPI: No active PM2.5 monitors found after filtering.")
        return(data.frame(site_number=character(), site_name=character()))
      }

      # Standardize site columns
      # Columns in monitors output are usually site_number and local_site_name
      sites <- monitor_data %>%
        dplyr::mutate(
          site_number = sprintf("%04d", as.numeric(.data[["site_number"]])),
          # Fallback for site name if local_site_name is missing
          site_name = if ("local_site_name" %in% colnames(.)) as.character(.data[["local_site_name"]]) else as.character(.data[["site_number"]])
        ) %>%
        dplyr::select(site_number, site_name) %>% 
        dplyr::arrange(site_number) %>%
        dplyr::distinct() 
      return(sites)
    } else {
      message("RAQSAPI: No PM2.5 monitors found for county: ", county_code)
      return(data.frame(site_number=character(), site_name=character())) 
    }
  }, error = function(e) {
    message("RAQSAPI Error (Monitors): ", e$message)
    print(e)
    return(data.frame(site_number=character(), site_name=character()))
  })
}

# ============================================================
# Module server
# ============================================================
dvServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  # Reactive values
  rv <- reactiveValues(
    states = NULL,
    counties = NULL,
    sites = NULL,
    data = NULL, # Main data frame holding fetched & potentially modified data
    original_data = NULL, # Store pristine fetched data
    excluded_dates = character(0), # Store dates as character YYYY-MM-DD for simplicity
    exclusion_history = list(), # Track exclusion changes
    events = data.frame( # Initialize event log structure
      start_date = as.Date(character()),
      end_date = as.Date(character()),
      event_type = character(),
      aqs_flag = character(),
      site_aqs_id = character(),
      site_name = character(),
      exceedance_concentration = numeric(), # Use numeric
      tier = character(),
      notes = character(),
      stringsAsFactors = FALSE
    ),
    date_range_count = 1, # For dynamic UI
    single_date_count = 1, # For dynamic UI
    date_range_values = list(), # Store UI state
    single_date_values = list(),  # Store UI state
    exclusion_states = list(), # Store states for undo/redo
    current_state_index = 0,
    hms_results = NULL # Store HMS smoke analysis results
  )
  
  # Load states when app starts
  observe({
    tryCatch({
      rv$states <- get_state_info()
      # No notification needed for hardcoded state usually
      # showNotification("Successfully loaded state data", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading states:", e$message), type = "error", duration = NULL)
    })
  })
  
  # Render state selector
  output$state_selector <- renderUI({
    req(rv$states)
    state_choices <- setNames(
      rv$states$state_code,
      paste(rv$states$state_code, "-", rv$states$state_name)
    )
    selectInput(ns("state"), "State:",
                choices = state_choices,
                selected = "28")  # Default to Mississippi
  })
  
  # Update counties when state changes
  observeEvent(input$state, {
    req(input$state)
    tryCatch({
      # Show progress/loading indicator
      withProgress(message = 'Loading counties...', value = 0.5, {
        rv$counties <- get_county_info(input$state)
      })
      if(is.null(rv$counties) || nrow(rv$counties) == 0) {
        showNotification("No counties found for the selected state.", type = "warning")
      } else {
        # showNotification("Successfully loaded county data", type = "message") # Can be noisy
      }
    }, error = function(e) {
      showNotification(paste("Error loading counties:", e$message), type = "error", duration = NULL)
      rv$counties <- NULL # Reset on error
    })
  })
  
  # Render county selector
  output$county_selector <- renderUI({
    req(rv$counties)
    if(nrow(rv$counties) == 0) return(p("No counties available.")) # Message if empty
    county_choices <- setNames(
      rv$counties$county_code,
      paste(rv$counties$county_code, "-", rv$counties$county_name)
    )
    # Try to default to DeSoto (033) or Bolivar (011) if they exist
    selected_county <- NULL
    if ("033" %in% rv$counties$county_code) selected_county <- "033"
    else if ("011" %in% rv$counties$county_code) selected_county <- "011"
    # Add more defaults or select the first one if specific ones aren't found
    else if (nrow(rv$counties) > 0) selected_county <- rv$counties$county_code[1]
    
    selectInput(ns("county"), "County:",
                choices = county_choices,
                selected = selected_county)
  })
  
  # Update sites when county changes
  observeEvent(input$county, {
    req(input$state, input$county)
    # Debounce this? If user clicks rapidly between counties.
    tryCatch({
      withProgress(message = 'Loading sites...', value = 0.5, {
        sites <- get_site_info(input$state, input$county)
      })
      rv$sites <- sites
      if(is.null(rv$sites) || nrow(rv$sites) == 0) {
        showNotification("No sites found for the selected county.", type = "warning")
      } else {
        # showNotification("Successfully loaded site data", type = "message") # Can be noisy
      }
    }, error = function(e) {
      # print(paste("Error in get_site_info observer:", e$message)) # Debug
      showNotification(paste("Error loading sites:", e$message), type = "error", duration = NULL)
      rv$sites <- NULL # Reset on error
    })
  })
  
  # Render site selector
  output$site_selector <- renderUI({
    req(rv$sites) # Require sites to be loaded
    if(nrow(rv$sites) == 0) return(p("No sites available.")) # Message if empty
    
    # print("Rendering site selector") # Debug
    # print(paste("Number of sites available:", nrow(rv$sites))) # Debug
    site_choices <- setNames(
      rv$sites$site_number,
      paste(rv$sites$site_number, "-", rv$sites$site_name)
    )
    # Select the first site by default if available
    selected_site <- if(nrow(rv$sites) > 0) rv$sites$site_number[1] else NULL
    selectInput(ns("site"), "Site:", choices = site_choices, selected = selected_site)
  })
  
  
  # Modify the data fetching observer
  observeEvent(input$fetch_data, {
    req(input$state, input$county, input$site, input$method_code, input$dv_year)
    # Basic validation
    if(is.na(input$dv_year) || input$dv_year < 2000 || input$dv_year > year(Sys.Date())) {
      showNotification("Invalid Design Value Year selected.", type="error")
      return()
    }
    
    withProgress(message = 'Fetching PM2.5 data...', value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Starting fetch...")
        end_year <- input$dv_year
        start_year <- end_year - 2
        # Note: Pass the *specific* method code selected in the UI
        # The fetch function needs to handle this - current one uses year-based logic
        # Adjust fetch_and_process_epa_data if it should strictly use input$method_code
        fetched_data <- fetch_and_process_epa_data(
          start_year, end_year,
          input$state, input$county, input$site,
          input$method_code # Pass selected code
        )
        
        incProgress(0.8, detail = "Processing data...")
        rv$data <- fetched_data
        rv$original_data <- fetched_data # Store a copy of the original data
        rv$excluded_dates <- character(0) # Reset exclusions on new fetch
        rv$data$excluded <- FALSE # Ensure excluded column is reset
        
        showNotification("Data fetched successfully", type = "message", duration = 5)
        incProgress(1, detail = "Done.")
        
        # --- HMS Smoke Analysis ---
        if (!is.null(fetched_data) && nrow(fetched_data) > 0) {
          high_days <- fetched_data %>%
            dplyr::filter(pm25 > 15) %>%
            dplyr::arrange(dplyr::desc(pm25))
          
          if (nrow(high_days) > 0) {
            # Limit to top 20 high days for performance
            high_days <- head(high_days, 20)
            
            # Create a progress indicator for smoke analysis
            withProgress(message = 'Checking HMS Smoke Impact...', value = 0, {
              smoke_impacts <- character(nrow(high_days))
              lat <- high_days$latitude[1]
              lon <- high_days$longitude[1]
              
              for (i in 1:nrow(high_days)) {
                incProgress(1/nrow(high_days), detail = paste("Checking", high_days$date[i]))
                smoke_sf <- get_hms_smoke(high_days$date[i])
                smoke_impacts[i] <- check_smoke_impact(smoke_sf, lat, lon)
              }
              
              high_days$Smoke_Impact <- smoke_impacts
              rv$hms_results <- high_days
              
              # Add Smoke_Impact back to rv$data
              rv$data <- rv$data %>%
                dplyr::left_join(
                  high_days %>% dplyr::select(date, Smoke_Impact),
                  by = "date"
                )
            })
          }
        }
        # --------------------------
        
      }, error = function(e) {
        showNotification(paste("Error fetching data:", e$message), type = "error", duration = NULL)
        rv$data <- NULL # Clear data on error
        rv$original_data <- NULL
      })
    })
    # Trigger recalculation explicitly after fetching new data
    # This ensures outputs update even if the user doesn't click "Recalculate"
    # Need to ensure rv$data is valid before triggering
    if (!is.null(rv$data) && nrow(rv$data) > 0) {
      # Use shinyjs::click("recalculate") or directly update outputs if preferred
      # For simplicity, let's rely on reactive dependencies or user clicking recalculate
    }
  })
  
  # Observer to update scenario year choices when data is loaded/updated
  observe({
    req(rv$data) # Requires rv$data to be non-NULL and have rows
    years <- sort(unique(rv$data$year), decreasing = TRUE)
    
    # Ensure years are valid before updating
    if (length(years) > 0) {
      # Default selection: last 2 years if available, otherwise just the last year
      selected_years <- head(years, 2)
      
      updateCheckboxGroupInput(session, "scenario_years",
                               choices = years,
                               selected = selected_years,
                               inline = TRUE)
    } else {
      # Clear choices if no valid years (e.g., data fetch failed)
      updateCheckboxGroupInput(session, "scenario_years", choices = character(0), selected = character(0))
    }
  })
  
  
  # Reactive for calculating Design Value (handles original and excluded)
  design_value_reactive <- reactive({
    # Require main data, original data (for comparison), and a valid DV year
    req(rv$data, rv$original_data, input$dv_year)
    shiny::validate(need(is.numeric(input$dv_year) && input$dv_year >= 2000, "Please select a valid Design Value Year."))
    shiny::validate(need(!is.null(rv$data) && nrow(rv$data) > 0, "No data loaded. Please fetch data first."))
    
    # Use a local copy to avoid modifying rv$data directly if calculate_design_value did
    current_data <- rv$data
    original_data_local <- rv$original_data
    
    # Calculate original DV (using the stored original data)
    # Ensure original_data has the 'excluded' column if needed by the function
    if (!"excluded" %in% names(original_data_local)) {
      original_data_local$excluded <- FALSE # Assume no exclusions in original
    }
    original_results <- calculate_design_value(original_data_local, input$dv_year)
    
    # Calculate DV with current exclusions (using rv$data which has the 'excluded' column)
    # The calculate_design_value function already filters based on the 'excluded' column
    excluded_results <- calculate_design_value(current_data, input$dv_year)
    
    # Return both sets of results
    list(
      original = original_results,
      excluded = excluded_results
    )
  })
  
  
  # Dynamic UI for Date Range Exclusions
  output$date_range_inputs <- renderUI({
    # Ensure count is at least 1
    num_ranges <- max(1, rv$date_range_count)
    lapply(seq_len(num_ranges), function(i) {
      id <- paste0("exclude_date_range_", i)
      # Use stored value or default to today
      start_val <- rv$date_range_values[[id]][[1]] %||% Sys.Date()
      end_val <- rv$date_range_values[[id]][[2]] %||% Sys.Date()
      dateRangeInput(
        inputId = ns(id),
        label = paste("Exclusion Date Range", i),
        start = start_val,
        end = end_val,
        max = Sys.Date() # Prevent future dates? Optional.
      )
    })
  })
  
  # Dynamic UI for Single Date Exclusions
  output$single_date_inputs <- renderUI({
    # Ensure count is at least 1
    num_singles <- max(1, rv$single_date_count)
    lapply(seq_len(num_singles), function(i) {
      id <- paste0("exclude_single_date_", i)
      # Use stored value or default to today
      date_val <- rv$single_date_values[[id]] %||% Sys.Date()
      dateInput(
        inputId = ns(id),
        label = paste("Exclude Single Date", i),
        value = date_val,
        max = Sys.Date() # Prevent future dates? Optional.
      )
    })
  })
  
  # Observers to store dynamic input values
  observe({
    lapply(seq_len(rv$date_range_count), function(i) {
      id <- paste0("exclude_date_range_", i)
      if (!is.null(input[[id]])) {
        # Store as Date objects
        rv$date_range_values[[id]] <- as.Date(input[[id]])
      }
    })
  })
  
  observe({
    lapply(seq_len(rv$single_date_count), function(i) {
      id <- paste0("exclude_single_date_", i)
      if (!is.null(input[[id]])) {
        # Store as Date object
        rv$single_date_values[[id]] <- as.Date(input[[id]])
      }
    })
  })
  
  # Add more date range inputs
  observeEvent(input$add_date_range, {
    rv$date_range_count <- rv$date_range_count + 1
    # Initialize new input's value store if needed (optional)
    new_id <- paste0("exclude_date_range_", rv$date_range_count)
    rv$date_range_values[[new_id]] <- c(Sys.Date(), Sys.Date())
  })
  
  # Add more single date inputs
  observeEvent(input$add_single_date, {
    rv$single_date_count <- rv$single_date_count + 1
    # Initialize new input's value store if needed (optional)
    new_id <- paste0("exclude_single_date_", rv$single_date_count)
    rv$single_date_values[[new_id]] <- Sys.Date()
  })
  
  # Apply Exclusions
  observeEvent(input$apply_exclusions, {
    req(rv$data) # Need data loaded
    
    # Store current state before applying new exclusions
    prev_excluded_count <- sum(rv$data$excluded)
    prev_excluded_dates <- rv$excluded_dates
    
    # Collect all dates to be excluded
    all_excluded_dates <- c()
    
    # Process date ranges
    for (i in seq_len(rv$date_range_count)) {
      id <- paste0("exclude_date_range_", i)
      range_input <- rv$date_range_values[[id]] # Use stored Date values
      if (!is.null(range_input) && length(range_input) == 2 && all(!is.na(range_input))) {
        # Generate sequence of dates within the range
        date_sequence <- seq(range_input[1], range_input[2], by = "day")
        all_excluded_dates <- c(all_excluded_dates, date_sequence)
      }
    }
    
    # Process single dates
    for (i in seq_len(rv$single_date_count)) {
      id <- paste0("exclude_single_date_", i)
      single_date <- rv$single_date_values[[id]] # Use stored Date value
      if (!is.null(single_date) && !is.na(single_date)) {
        all_excluded_dates <- c(all_excluded_dates, single_date)
      }
    }
    
    # Ensure unique dates and store as character for consistency with previous logic if needed
    # Or keep as Date objects if comparison below uses Date objects
    unique_dates <- unique(as.Date(all_excluded_dates, origin = "1970-01-01"))
    rv$excluded_dates <- format(unique_dates, "%Y-%m-%d") # Store as character
    
    # Apply exclusions to the main data frame (rv$data)
    # Ensure comparison is between Date objects or consistent character formats
    rv$data$excluded <- as.Date(rv$data$date) %in% unique_dates
    
    # Record the change in exclusion history
    new_excluded_count <- sum(rv$data$excluded)
    if (new_excluded_count != prev_excluded_count) {
      change_record <- list(
        timestamp = Sys.time(),
        action = "Manual Exclusion",
        previous_count = prev_excluded_count,
        new_count = new_excluded_count,
        dates_added = setdiff(rv$excluded_dates, prev_excluded_dates),
        dates_removed = setdiff(prev_excluded_dates, rv$excluded_dates)
      )
      rv$exclusion_history <- append(rv$exclusion_history, list(change_record))
    }
    
    # Provide feedback
    num_applied = sum(rv$data$excluded)
    showNotification(paste("Applied", length(unique_dates), "unique exclusion dates.", num_applied, "data points marked."), type = "warning", duration = 5)
    
    # Print debugging information (optional)
    # print(paste("Number of unique excluded dates:", length(unique_dates)))
    # print(paste("First few excluded dates:", head(rv$excluded_dates)))
    # print(paste("Number of excluded rows in data:", sum(rv$data$excluded)))
    
    # Suggest recalculation
    showNotification("Exclusions applied. Click 'Recalculate Design Value' to see the impact.", duration = 8)
    save_exclusion_state()
    
  })
  
  # Reset Exclusions
  observeEvent(input$reset, {
    rv$excluded_dates <- character(0) # Clear stored dates
    if (!is.null(rv$data)) {
      rv$data$excluded <- FALSE # Reset flag in the data frame
    }
    # Reset dynamic UI counts and stored values
    rv$date_range_count <- 1
    rv$single_date_count <- 1
    rv$date_range_values <- list(exclude_date_range_1 = c(Sys.Date(), Sys.Date())) # Reset to default
    rv$single_date_values <- list(exclude_single_date_1 = Sys.Date()) # Reset to default
    
    showNotification("All exclusions have been reset.", type = "message", duration = 5)
    # Suggest recalculation after reset
    showNotification("Exclusions reset. Click 'Recalculate Design Value' to see the original value.", duration = 8)
  })
  
  # Add these observers after the reset observer
  observeEvent(input$undo_exclusion, {
    if (rv$current_state_index > 1) {
      rv$current_state_index <- rv$current_state_index - 1
      state <- rv$exclusion_states[[rv$current_state_index]]
      rv$data$excluded <- state$excluded
      rv$excluded_dates <- state$excluded_dates
      showNotification("Undid last exclusion change", type = "info")
    } else {
      showNotification("Nothing to undo", type = "warning")
    }
  })
  
  observeEvent(input$redo_exclusion, {
    if (rv$current_state_index < length(rv$exclusion_states)) {
      rv$current_state_index <- rv$current_state_index + 1
      state <- rv$exclusion_states[[rv$current_state_index]]
      rv$data$excluded <- state$excluded
      rv$excluded_dates <- state$excluded_dates
      showNotification("Redid exclusion change", type = "info")
    } else {
      showNotification("Nothing to redo", type = "warning")
    }
  })
  
  # Add Event to Log
  observeEvent(input$add_event, {
    # Basic validation
    if(is.null(input$event_date_range) || any(is.na(input$event_date_range))) {
      showNotification("Please select a valid date range for the event.", type="error")
      return()
    }
    if(input$event_type == "") {
      showNotification("Please enter an Event Type.", type="error")
      return()
    }
    
    # Get site info from main selection (if data is loaded)
    site_id_val <- if (!is.null(rv$data)) paste(unique(rv$data$state_code), unique(rv$data$county_code), unique(rv$data$site_number), sep="-") else "N/A"
    site_name_val <- if (!is.null(rv$data)) unique(rv$data$local_site_name) else "N/A"
    
    new_event <- data.frame(
      start_date = as.Date(input$event_date_range[1]),
      end_date = as.Date(input$event_date_range[2]),
      event_type = input$event_type,
      aqs_flag = input$aqs_flag,
      site_aqs_id = site_id_val, # Auto-fill attempt
      site_name = site_name_val, # Auto-fill attempt
      exceedance_concentration = as.numeric(input$exceedance_concentration), # Ensure numeric
      tier = input$tier,
      notes = input$notes,
      stringsAsFactors = FALSE
    )
    
    # Add to the reactive event log
    rv$events <- rbind(rv$events, new_event)
    showNotification("Event added to log.", type="success", duration = 5)
    
    # Optionally, automatically apply these dates as exclusions
    apply_event_dates <- TRUE # Make this a user choice? (e.g., checkbox "Apply dates as exclusions")
    if(apply_event_dates && !is.null(rv$data)) {
      event_date_seq <- seq(new_event$start_date, new_event$end_date, by = "day")
      current_excluded_dates <- as.Date(rv$excluded_dates, origin="1970-01-01") # Convert existing
      combined_dates <- unique(c(current_excluded_dates, event_date_seq))
      rv$excluded_dates <- format(combined_dates, "%Y-%m-%d") # Store back as character
      rv$data$excluded <- as.Date(rv$data$date) %in% combined_dates # Update data frame
      showNotification(paste("Dates from event automatically added to exclusions. Recalculate DV."), type = "info", duration = 8)
    }
  })
  
  
  # --- START OF CRASH FIX ---
  
  # Recalculate Button Observer (incorporating the fix)
  # --- START OF CRASH FIX ---
  
  # Recalculate Button Observer (incorporating the fix)
  observeEvent(input$recalculate, {
    # Requirements: data must be loaded, original exists, valid year selected
    req(rv$data, rv$original_data, input$dv_year)
    shiny::validate(need(!is.null(rv$data) && nrow(rv$data) > 0, "No data loaded to recalculate."))
    shiny::validate(need(is.numeric(input$dv_year), "Invalid DV year selected."))
    
    # Calculate results ONCE using the reactive expression
    results <- design_value_reactive()
    
    # Check for calculation errors in either original or excluded results
    if (!is.null(results$original$error)) {
      showNotification(paste("Error calculating original DV:", results$original$error), type = "error", duration = NULL)
    }
    if (!is.null(results$excluded$error)) {
      showNotification(paste("Error calculating DV with exclusions:", results$excluded$error), type = "error", duration = NULL)
    }
    
    # Check for method transition flag and show notification
    # Initialize flag
    has_transition_flag <- FALSE
    # Check original results if valid
    if (!is.null(results$original) && !is.null(results$original$has_method_transition) && isTRUE(results$original$has_method_transition)) {
      has_transition_flag <- TRUE
    }
    # Check excluded results if valid (only if different from original or if original failed)
    if (!is.null(results$excluded) && !is.null(results$excluded$has_method_transition) && isTRUE(results$excluded$has_method_transition)) {
      has_transition_flag <- TRUE # Set flag if transition occurs in either calc
    }
    
    # Show notification if the flag was set
    if (has_transition_flag) {
      showNotification(
        "Note: Design value calculation includes data from different method codes (likely pre/post 2024 transition).",
        type = "warning",
        duration = 10 # Show for 10 seconds or NULL to persist
      )
    }
    # --- End of Method Transition Check ---
    
    
    # Update the 'Recalculated Design Value' text output
    output$recalculated_design_value <- renderText({
      # Use the results calculated above
      req(results)
      
      # Format original DV safely
      original_dv_text <- if (!is.null(results$original) && !is.null(results$original$design_value) && !is.na(results$original$design_value)) {
        paste0(format(round(results$original$design_value, 1), nsmall = 1), " µg/m³")
      } else {
        "Not Available" # Or show error message if available: results$original$error
      }
      
      # Format excluded DV safely
      excluded_dv_text <- if (!is.null(results$excluded) && !is.null(results$excluded$design_value) && !is.na(results$excluded$design_value)) {
        paste0(format(round(results$excluded$design_value, 1), nsmall = 1), " µg/m³")
      } else {
        # Provide context: Was it not calculated, or just same as original?
        if (!any(rv$data$excluded)) "(No exclusions applied)" else "Not Available" # Or show error: results$excluded$error
      }
      
      paste(
        input$dv_year, "Design Value (Before Exclusions):", original_dv_text, "\n",
        input$dv_year, "Design Value (After Exclusions): ", excluded_dv_text
      )
    })
    
    # Add validation summary output
    output$dv_validation_summary <- renderUI({
      req(results)
      
      # Create validation checks
      validation_items <- list()
      
      # Check 1: Verify excluded data points count
      excluded_count <- sum(rv$data$excluded)
      validation_items <- c(validation_items, 
                            tags$li(paste("Excluded data points:", excluded_count))
      )
      
      # Check 2: Verify annual means calculation
      if (!is.null(results$excluded$annual_means)) {
        for(i in 1:nrow(results$excluded$annual_means)) {
          year_data <- results$excluded$annual_means[i,]
          validation_items <- c(validation_items,
                                tags$li(paste0("Year ", year_data$year, ": ", 
                                               year_data$n_observations, " observations, ",
                                               round(year_data$data_completeness, 1), "% complete, ",
                                               "Mean: ", round(year_data$annual_mean, 2), " µg/m³"))
          )
        }
      }
      
      # Check 3: Show DV calculation
      if (!is.null(results$excluded$design_value) && !is.na(results$excluded$design_value)) {
        means <- results$excluded$annual_means$annual_mean
        calc_dv <- mean(means, na.rm = TRUE)
        validation_items <- c(validation_items,
                              tags$li(HTML(paste0("<strong>DV Calculation: (", 
                                                  paste(round(means, 2), collapse=" + "), 
                                                  ") / 3 = ", 
                                                  round(calc_dv, 2), " µg/m³</strong>")))
        )
      }
      
      div(
        h5("Validation Summary:"),
        tags$ul(validation_items),
        style = "background-color: #f0f0f0; padding: 10px; margin-top: 10px; border-radius: 5px;"
      )
    })
    
    # Add completeness check and warning
    if (!is.null(results$excluded$annual_means)) {
      low_completeness_years <- results$excluded$annual_means %>%
        dplyr::filter(data_completeness < 75) %>%
        dplyr::pull(year)
      
      if (length(low_completeness_years) > 0) {
        showNotification(
          HTML(paste0(
            "<strong>Warning:</strong> The following years have less than 75% data completeness:<br>",
            paste(low_completeness_years, collapse = ", "), "<br>",
            "This may affect the validity of the design value calculation."
          )),
          type = "warning",
          duration = 10
        )
      }
    }
    
    # Update the data completeness output (use excluded results after recalculation)
    output$data_completeness_output <- renderText({
      # Use the results calculated above
      req(results, results$excluded, results$excluded$annual_means)
      shiny::validate(need(nrow(results$excluded$annual_means) > 0, "Cannot calculate completeness (no annual means)."))
      
      tryCatch({
        completeness_df <- results$excluded$annual_means %>%
          # Ensure required columns exist
          dplyr::select(year, data_completeness, n_observations) %>%
          dplyr::mutate(completeness_text = sprintf("%d: %.1f%% (%d obs.)",
                                             year,
                                             round(data_completeness, 1),
                                             n_observations))
        
        completeness_string <- paste(completeness_df$completeness_text, collapse = "\n")
        
        paste("Data Completeness (After Exclusions):\n", completeness_string)
      }, error = function(e) {
        paste("Error calculating data completeness:", e$message)
      })
    })
    
    # Update the excluded dates table display as well
    output$excluded_dates_table <- renderDT({
      req(rv$data, rv$excluded_dates) # Require data and the list of excluded dates
      
      # Filter the main data (rv$data) for dates that are in the rv$excluded_dates list
      # and within the current DV period
      dv_years <- (input$dv_year - 2):input$dv_year
      excluded_data_to_show <- rv$data %>%
        dplyr::filter(excluded == TRUE, year %in% dv_years) %>% # Filter using the 'excluded' flag
        dplyr::select(date, pm25, year) %>% # Select relevant columns
        dplyr::arrange(desc(date)) %>% # Show most recent first
        dplyr::mutate(date = format(as.Date(date), "%Y-%m-%d"), # Format date for display
               pm25 = round(pm25, 1)) # Round PM2.5
      
      if(nrow(excluded_data_to_show) > 0) {
        datatable(excluded_data_to_show,
                  colnames = c("Excluded Date", "PM2.5 Value", "Year"),
                  rownames = FALSE,
                  options = list(pageLength = 10, order = list(list(0, 'desc')))) # Order by date descending
      } else {
        # Show an empty table with correct headers if nothing is excluded
        datatable(data.frame(`Excluded Date` = character(0), `PM2.5 Value` = numeric(0), `Year`= integer(0)),
                  rownames = FALSE,
                  options = list(pageLength = 10))
      }
    })
    
    
  }) # End of observeEvent(input$recalculate, ...)
  
  # --- END OF CRASH FIX ---
  
  
  # Display initial/recalculated Design Value in the main overview box
  output$design_value_output <- renderText({
    req(rv$data, input$dv_year) # Require data
    # Use the reactive expression to get calculated values
    results <- design_value_reactive()
    shiny::validate(need(results, "Calculating design value...")) # Placeholder message
    
    # Check for errors during calculation
    if (!is.null(results$original$error) || !is.null(results$excluded$error)) {
      error_msg <- paste(
        "Original Calc Error:", results$original$error %||% "None", "\n",
        "Excluded Calc Error:", results$excluded$error %||% "None"
      )
      return(paste("Error calculating design value:\n", error_msg))
    }
    
    # Format the output string
    output_text <- "Current Calculation:\n"
    
    # Original Value
    output_text <- paste0(output_text, "  Before Exclusions: ",
                          ifelse(!is.na(results$original$design_value),
                                 paste0(format(round(results$original$design_value, 1), nsmall = 1), " µg/m³"),
                                 "Not Available"), "\n")
    
    # Value After Exclusions (if different)
    if (any(rv$data$excluded)) { # Only show if exclusions are applied
      output_text <- paste0(output_text, "  After Exclusions:  ",
                            ifelse(!is.na(results$excluded$design_value),
                                   paste0(format(round(results$excluded$design_value, 1), nsmall = 1), " µg/m³"),
                                   "Not Available"), "\n")
    } else {
      output_text <- paste0(output_text, "  (No exclusions currently applied)\n")
    }
    
    # Add Annual Means Summary (from the 'excluded' calculation, which reflects current state)
    if (!is.null(results$excluded$annual_means) && nrow(results$excluded$annual_means) > 0) {
      output_text <- paste0(output_text, "\nAnnual Means (Current View):\n")
      means_summary <- results$excluded$annual_means %>%
        dplyr::mutate(summary = sprintf("  %d: %.1f µg/m³", year, round(annual_mean, 1))) %>%
        dplyr::pull(summary) %>%
        paste(collapse = "\n")
      output_text <- paste0(output_text, means_summary)
    } else {
      output_text <- paste0(output_text, "\nAnnual Means: Not available.")
    }
    
    output_text # Return the formatted string
  })
  
  
  # Data Completeness Output (Original verbatimTextOutput - keep or replace with valueBox)
  output$data_completeness_output <- renderText({
    req(rv$data, input$dv_year)
    results <- design_value_reactive() # Use the reactive expression
    shiny::validate(need(results, "Calculating completeness..."))
    
    # Use the 'excluded' results as they reflect the current view
    if (is.null(results$excluded) || is.null(results$excluded$annual_means) || nrow(results$excluded$annual_means) == 0) {
      return("Data completeness not available (check data/exclusions).")
    }
    if (!is.null(results$excluded$error)) {
      return(paste("Cannot calculate completeness due to error:", results$excluded$error))
    }
    
    
    tryCatch({
      completeness_df <- results$excluded$annual_means %>%
        # Ensure required columns exist
        dplyr::select(year, data_completeness, n_observations) %>%
        # Format string for each year
        dplyr::mutate(completeness_text = sprintf("%d: %.1f%% (%d obs.)",
                                           year,
                                           round(data_completeness, 1),
                                           n_observations))
      
      # Combine strings for all years
      completeness_string <- paste(completeness_df$completeness_text, collapse = "\n")
      
      # Final output string
      paste("Data Completeness (Current View):\n", completeness_string)
      
    }, error = function(e) {
      paste("Error formatting data completeness:", e$message)
    })
  })
  
  
  # Value Box for Design Value
  output$design_value_box <- renderUI({
    req(rv$data)
    results <- design_value_reactive()
    dv <- NA # Default
    subtitle_text <- "(Before Exclusions)"
    box_color <- "blue" # Default color
    
    if(!is.null(results)) {
      # Use excluded value if exclusions applied, otherwise original
      if(any(rv$data$excluded) && !is.null(results$excluded)) {
        dv <- results$excluded$design_value
        subtitle_text <- "(After Exclusions)"
        box_color <- "orange" # Change color if exclusions applied
      } else if (!is.null(results$original)) {
        dv <- results$original$design_value
      }
      
      # Handle calculation errors
      if ((any(rv$data$excluded) && !is.null(results$excluded$error)) || (!any(rv$data$excluded) && !is.null(results$original$error)) ) {
        dv <- "Error"
        box_color <- "red"
      } else if (is.na(dv)) {
        dv <- "NA" # Display NA if calculation resulted in NA
        box_color <- "gray"
      } else {
        dv <- format(round(dv, 1), nsmall = 1) # Format if numeric
      }
    } else {
      dv <- "Loading..."
      box_color <- "gray"
    }
    
    
    bslib::value_box(
      title    = paste(input$dv_year, "Design Value", subtitle_text),
      value    = dv,
      showcase = icon("tachometer-alt"),
      theme    = unname(c(blue="primary", orange="warning", red="danger",
                          gray="secondary", green="success", purple="info")[box_color])
    )
  })
  
  # Value Box for Average Data Completeness (Example)
  output$data_completeness_box <- renderUI({
    req(rv$data)
    results <- design_value_reactive()
    completeness_val <- "NA" # Default
    subtitle_text <- "(Current View)"
    box_color <- "purple" # Default color
    
    if(!is.null(results) && !is.null(results$excluded) && !is.null(results$excluded$annual_means) && nrow(results$excluded$annual_means) > 0 && is.null(results$excluded$error)) {
      # Calculate average completeness over the 3 years
      avg_comp <- mean(results$excluded$annual_means$data_completeness, na.rm = TRUE)
      if (!is.na(avg_comp)) {
        completeness_val <- paste0(round(avg_comp, 1), "%")
        # Change color based on completeness threshold? e.g., < 75%
        if(avg_comp < 75) box_color <- "red" else box_color <- "green"
      }
    } else if (!is.null(results$excluded$error)) {
      completeness_val <- "Error"
      box_color <- "red"
    } else {
      completeness_val <- "Loading..."
      box_color <- "gray"
    }
    
    
    bslib::value_box(
      title    = paste("Avg Data Completeness", subtitle_text),
      value    = completeness_val,
      showcase = icon("check-circle"),
      theme    = unname(c(blue="primary", orange="warning", red="danger",
                          gray="secondary", green="success", purple="info")[box_color])
    )
  })
  
  
  # Data Table Output
  output$data_table <- renderDT({
    req(rv$data)
    shiny::validate(need(!is.null(rv$data) && nrow(rv$data) > 0, "No data available. Please fetch data first."))
    # Select and arrange columns for display
    display_data <- rv$data %>%
      dplyr::mutate(date = format(as.Date(date), "%Y-%m-%d"), # Format date
             pm25 = round(pm25, 1))
    
    # Conditionally include Smoke_Impact if it exists
    if ("Smoke_Impact" %in% colnames(display_data)) {
      display_data <- display_data %>%
        dplyr::select(date, year, pm25, Smoke_Impact, method_code, method_name, excluded)
      colnames_vec <- c("Date", "Year", "PM2.5", "Smoke Impact", "Method Code", "Method Name", "Excluded?")
    } else {
      display_data <- display_data %>%
        dplyr::select(date, year, pm25, method_code, method_name, excluded)
      colnames_vec <- c("Date", "Year", "PM2.5", "Method Code", "Method Name", "Excluded?")
    }
    
    display_data <- display_data %>% dplyr::arrange(desc(date)) # Show most recent first
    
    datatable(display_data,
              rownames = FALSE,
              colnames = colnames_vec,
              options = list(pageLength = 10,
                             order = list(list(0, 'desc')), # Order by Date descending
                             scrollX = TRUE
              )
    ) %>%
      formatStyle('excluded',
                  target = 'row',
                  backgroundColor = styleEqual(c(TRUE), c('#f2dede'))
      )
  })

  # --- HMS Smoke Outputs ---
  output$hms_high_days_table <- renderDT({
    req(rv$hms_results)
    
    display_hms <- rv$hms_results %>%
      dplyr::select(date, pm25, Smoke_Impact) %>%
      dplyr::mutate(date = format(as.Date(date), "%Y-%m-%d"),
                    pm25 = round(pm25, 1)) %>%
      dplyr::arrange(desc(pm25))
    
    datatable(display_hms,
              selection = 'single', # Only one day at a time for map
              rownames = FALSE,
              colnames = c("Date", "PM2.5 Conc (µg/m³)", "Smoke Density at Site"),
              options = list(pageLength = 5)
    )
  })

  output$hms_smoke_map <- renderLeaflet({
    # Base map (no labels)
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = -89.5, lat = 32.7, zoom = 7)
    
    # 1. Add smoke polygons FIRST (so labels go on top)
    s <- input$hms_high_days_table_rows_selected
    if (length(s) > 0) {
      selected_date <- rv$hms_results$date[s]
      smoke_sf <- get_hms_smoke(selected_date)
      
      if (!is.null(smoke_sf)) {
        # Define color palette for smoke density
        pal <- colorFactor(
          palette = c("yellow", "orange", "red"),
          levels = c("Light", "Medium", "Heavy")
        )
        
        m <- m %>% addPolygons(data = smoke_sf,
                               color = "black", weight = 1,
                               fillColor = ~pal(Density),
                               fillOpacity = 0.5, # Slightly more opaque
                               label = ~paste("Smoke Density:", Density)) %>%
                   addLegend(pal = pal, values = smoke_sf$Density, 
                             title = paste("Smoke on", selected_date))
      }
    }
    
    # 2. Add labels and boundaries ON TOP of everything
    m <- m %>% addProviderTiles(providers$CartoDB.PositronOnlyLabels)
    
    # 3. Add site marker
    if (!is.null(rv$data) && nrow(rv$data) > 0) {
      lat <- rv$data$latitude[1]
      lon <- rv$data$longitude[1]
      m <- m %>% addCircleMarkers(lng = lon, lat = lat, 
                                  color = "blue", radius = 8, 
                                  weight = 3, opacity = 1,
                                  fillColor = "white", fillOpacity = 0.8,
                                  label = "Monitoring Site")
    }
    
    m
  })
  # --------------------------
  
  # Annual Means Plot (Now reactive to data changes)
  output$annual_means_plot <- renderPlot({
    # Use the reactive expression to get the latest calculations
    results <- design_value_reactive()
    
    # Require the results and the annual_means table to exist
    req(results, results$excluded, results$excluded$annual_means)
    
    # Ensure there's data to plot
    shiny::validate(need(nrow(results$excluded$annual_means) > 0, "Annual means data is not available. Check data fetch or exclusion criteria."))
    
    # Use the 'excluded' results as they represent the current state of the data
    plot_data <- results$excluded$annual_means
    
    # Determine plot title based on whether exclusions are active
    plot_title <- if(any(rv$data$excluded)) {
      paste("PM2.5 Annual Means for", input$dv_year, "Design Value Period (After Exclusions)")
    } else {
      paste("PM2.5 Annual Means for", input$dv_year, "Design Value Period")
    }
    
    ggplot(plot_data, aes(x = factor(year), y = annual_mean, group = 1)) +
      geom_line(color = "#00c0ef", size = 1.2) + # Match the box color
      geom_point(color = "#00c0ef", size = 4) +
      geom_text(aes(label = round(annual_mean, 2)), vjust = -1.5, size = 4, color = "black", fontface = "bold") +
      geom_hline(yintercept = 9.0, linetype = "dashed", color = "red") +
      labs(title = plot_title,
           x = "Year", y = "Annual Mean PM2.5 (µg/m³)") +
      theme_minimal(base_size = 14) +
      scale_y_continuous(limits = c(0, max(plot_data$annual_mean, 9.5, na.rm = TRUE) * 1.2)) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            panel.grid.minor = element_blank()) # Center title and clean up grid
  })
  
  
  # Calendar Heatmaps
  output$calendar_heatmap_year1 <- renderPlotly({
    req(rv$data, input$dv_year)
    shiny::validate(need(!is.null(rv$data) && nrow(rv$data) > 0, "No data available. Please fetch data first."))
    year_to_plot <- input$dv_year - 2
    create_calendar_heatmap(rv$data, year_to_plot, paste("PM2.5 Heatmap -", year_to_plot))
  })
  
  output$calendar_heatmap_year2 <- renderPlotly({
    req(rv$data, input$dv_year)
    shiny::validate(need(!is.null(rv$data) && nrow(rv$data) > 0, "No data available. Please fetch data first."))
    year_to_plot <- input$dv_year - 1
    create_calendar_heatmap(rv$data, year_to_plot, paste("PM2.5 Heatmap -", year_to_plot))
  })
  
  output$calendar_heatmap_year3 <- renderPlotly({
    req(rv$data, input$dv_year)
    shiny::validate(need(!is.null(rv$data) && nrow(rv$data) > 0, "No data available. Please fetch data first."))
    year_to_plot <- input$dv_year
    create_calendar_heatmap(rv$data, year_to_plot, paste("PM2.5 Heatmap -", year_to_plot))
  })
  
  
  # Excluded Dates Table (now updated within recalculate observer)
  # output$excluded_dates_table <- renderDT({ ... })
  
  
  # Event Log Table
  output$event_log_table <- renderDT({
    # Ensure dates are formatted nicely
    display_events <- rv$events %>%
      dplyr::mutate(start_date = format(as.Date(start_date), "%Y-%m-%d"),
             end_date = format(as.Date(end_date), "%Y-%m-%d"))
    datatable(display_events, rownames = FALSE,
              options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Download Event Log
  output$download_event_log <- downloadHandler(
    filename = function() {
      site_id <- if (!is.null(rv$data)) paste0(unique(rv$data$county_code),"-",unique(rv$data$site_number)) else "events"
      paste0("event_log_", site_id, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$events, file, row.names = FALSE)
    }
  )
  
  
  # Download Complete Analysis Report
  output$download_analysis_report <- downloadHandler(
    filename = function() {
      site_id <- if (!is.null(rv$data)) {
        paste0(unique(rv$data$county_code), "-", unique(rv$data$site_number))
      } else "analysis"
      paste0("dv_analysis_", site_id, "_", input$dv_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$data)
      results <- design_value_reactive()
      
      # Create summary data
      summary_data <- data.frame(
        Parameter = c("Site", "Design Value Year", "Original DV", "Recalculated DV", 
                      "Excluded Points", "Date Generated"),
        Value = c(
          paste(unique(rv$data$county_code), "-", unique(rv$data$site_number)),
          input$dv_year,
          ifelse(!is.null(results$original$design_value), 
                 round(results$original$design_value, 2), "NA"),
          ifelse(!is.null(results$excluded$design_value), 
                 round(results$excluded$design_value, 2), "NA"),
          sum(rv$data$excluded),
          as.character(Sys.Date())
        )
      )
      
      # Combine with detailed data
      detailed_data <- rv$data %>%
        dplyr::select(date, year, pm25, method_code, excluded) %>%
        dplyr::arrange(date)
      
      # Write multiple sheets or sections
      write.csv(summary_data, file, row.names = FALSE)
      write.table("\n\nDetailed Data:\n", file, append = TRUE, 
                  col.names = FALSE, row.names = FALSE, quote = FALSE)
      write.table(detailed_data, file, append = TRUE, 
                  row.names = FALSE, sep = ",")
    }
  )
  
  # --- Scenario Analysis ---
  scenario_results_reactive <- reactiveVal(NULL) # Store results of the scenario run
  
  # Run Scenario Analysis
  # Run Scenario Analysis
  observeEvent(input$run_scenario, {
    req(rv$data, input$dv_year, input$target_dv, input$scenario_years)
    shiny::validate(need(!is.null(rv$data) && nrow(rv$data) > 0, "Load data before running scenario."))
    shiny::validate(need(is.numeric(input$target_dv) && input$target_dv >= 0, "Enter a valid numeric Target Design Value."))
    shiny::validate(need(length(input$scenario_years) > 0, "Select at least one year for potential exclusions."))
    
    # Re-enable the apply button in case it was previously disabled
    shinyjs::enable("apply_scenario")
    
    withProgress(message = 'Running scenario...', value = 0, {
      # Use a temporary copy of the data for the simulation
      scenario_sim_data <- rv$data
      scenario_sim_data$excluded <- rv$data$excluded # Start with current exclusions
      
      # Calculate initial DV based on current exclusions in the temp data
      initial_calc <- calculate_design_value(scenario_sim_data, input$dv_year)
      current_dv <- initial_calc$design_value
      
      # Store original DV before scenario simulation
      original_dv_before_scenario <- initial_calc$design_value
      
      # Store dates identified for exclusion in this scenario run
      scenario_exclusions_list <- list()
      processed_iterations = 0
      max_iterations = nrow(scenario_sim_data) # Safety break
      
      incProgress(0.1, detail = paste("Initial DV:", round(current_dv,1)))
      
      # Loop while DV is above target and we haven't exceeded iterations
      while (!is.na(current_dv) && current_dv > input$target_dv && processed_iterations < max_iterations) {
        processed_iterations <- processed_iterations + 1
        incProgress(0.05, detail = paste("Iteration", processed_iterations)) 
        
        # Find the highest PM2.5 value that's:
        # 1. Not already excluded in the *simulation* data
        # 2. Within the user-selected scenario years
        # 3. Within the 3-year DV period
        potential_exclusions <- scenario_sim_data %>%
          dplyr::filter(!excluded, 
                 year %in% as.numeric(input$scenario_years), 
                 year >= (input$dv_year - 2) & year <= input$dv_year) 
        
        if(nrow(potential_exclusions) == 0) {
          incProgress(1, detail = "No more valid points to exclude.")
          break 
        }
        
        # Identify the highest point among potential exclusions
        highest_point <- potential_exclusions %>% dplyr::arrange(desc(pm25)) %>% dplyr::slice(1)
        
        # Mark this point as excluded *in the simulation data*
        scenario_sim_data$excluded[scenario_sim_data$date == highest_point$date] <- TRUE
        
        # Recalculate design value *using the simulation data*
        new_calc <- calculate_design_value(scenario_sim_data, input$dv_year)
        new_dv <- new_calc$design_value
        
        # Calculate impact
        impact <- current_dv - new_dv
        
        # Identify columns to select (preserving Smoke_Impact if it exists)
        cols_to_keep <- c("date", "year", "pm25")
        if ("Smoke_Impact" %in% colnames(highest_point)) cols_to_keep <- c(cols_to_keep, "Smoke_Impact")
        
        # Add the excluded point to our scenario list with impact
        scenario_exclusions_list[[length(scenario_exclusions_list) + 1]] <- highest_point %>% 
          dplyr::select(dplyr::all_of(cols_to_keep)) %>%
          dplyr::mutate(dv_impact = impact, resulting_dv = new_dv)
        
        current_dv <- new_dv
        
        # Check for errors during recalculation within the loop
        if (!is.null(new_calc$error)) {
          showNotification(paste("Error during scenario iteration:", new_calc$error), type = "error")
          incProgress(1, detail = "Error encountered.")
          current_dv <- NA 
          break
        }
      }
      
      # Combine the list of excluded points into a data frame
      scenario_exclusions_df <- if(length(scenario_exclusions_list) > 0) {
        dplyr::bind_rows(scenario_exclusions_list)
      } else {
        data.frame(date = as.Date(character()), year = integer(), pm25 = numeric(), dv_impact = numeric(), resulting_dv = numeric())
      }
      
      # Store the results in the reactiveVal
      scenario_results_reactive(list(
        original_dv = original_dv_before_scenario, # DV before this scenario started
        final_dv = current_dv, # DV after simulation finished
        target_dv = input$target_dv,
        exclusions_df = scenario_exclusions_df,
        iterations = processed_iterations,
        status = if (is.na(current_dv)) "Error" else if (current_dv <= input$target_dv) "Target Met" else "Target Not Met (No more points)"
      ))
      
      incProgress(1, detail = "Scenario complete.")
    }) # End withProgress
  })
  
  
  # Display Scenario Results Text
  output$scenario_results <- renderText({
    results <- scenario_results_reactive() # Get results from reactiveVal
    req(results) # Require results to be available
    
    # Build the summary string
    summary_text <- paste0(
      "Scenario Status: ", results$status, "\n",
      "Target Design Value: ", format(round(results$target_dv, 1), nsmall = 1), " µg/m³\n",
      "Design Value Before Scenario Run: ", ifelse(is.na(results$original_dv), "NA", format(round(results$original_dv, 1), nsmall = 1)), " µg/m³\n",
      "Design Value After Scenario Run:  ", ifelse(is.na(results$final_dv), "NA", format(round(results$final_dv, 1), nsmall = 1)), " µg/m³\n",
      "Number of additional exclusions identified: ", nrow(results$exclusions_df), "\n"
      # Add breakdown by year if needed
    )
    
    if(nrow(results$exclusions_df) > 0) {
      exclusions_by_year <- results$exclusions_df %>%
        dplyr::group_by(year) %>%
        dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
        dplyr::mutate(text = paste0("  - Year ", year, ": ", count, " exclusion(s)")) %>%
        dplyr::pull(text) %>%
        paste(collapse = "\n")
      summary_text <- paste0(summary_text, "Exclusions by Year:\n", exclusions_by_year, "\n")
    }
    
    summary_text # Return the complete summary
  })
  
  # Display Scenario Exclusions Table
  output$scenario_table <- renderDT({
    results <- scenario_results_reactive()
    req(results, results$exclusions_df) 
    
    if(nrow(results$exclusions_df) > 0) {
      display_df <- results$exclusions_df %>%
        dplyr::mutate(date = format(as.Date(date), "%Y-%m-%d"), 
               pm25 = round(pm25, 1),
               dv_impact = round(dv_impact, 3),
               resulting_dv = round(resulting_dv, 2))
      
      # Dynamically adjust columns for Smoke_Impact
      if ("Smoke_Impact" %in% colnames(display_df)) {
        display_df <- display_df %>% 
          dplyr::select(date, year, pm25, Smoke_Impact, dv_impact, resulting_dv)
        colnames_vec <- c("Date Identified", "Year", "PM2.5 Value", "Smoke Impact", "DV Impact", "Resulting DV")
      } else {
        display_df <- display_df %>% 
          dplyr::select(date, year, pm25, dv_impact, resulting_dv)
        colnames_vec <- c("Date Identified", "Year", "PM2.5 Value", "DV Impact", "Resulting DV")
      }
      
      display_df <- display_df %>% dplyr::arrange(desc(dv_impact))
      
      datatable(display_df,
                rownames = FALSE,
                colnames = colnames_vec,
                options = list(pageLength = 10, order = list(list(ncol(display_df)-2, 'desc')))) # Order by Impact
    } else {
      # Show empty table if no exclusions were needed/identified
      datatable(data.frame(`Date Identified` = character(0), Year = integer(0), `PM2.5 Value` = numeric(0), `DV Impact` = numeric(0), `Resulting DV` = numeric(0)),
                rownames = FALSE, options = list(pageLength = 10))
    }
  })
  
  # Apply Scenario Exclusions to Main Data
  observeEvent(input$apply_scenario, {
    results <- scenario_results_reactive() # Get the results from the last run
    req(results, results$exclusions_df) # Need the results and the exclusion list
    req(rv$data) # Need the main data to apply to
    
    if (nrow(results$exclusions_df) == 0) {
      showNotification("No exclusions identified in the scenario to apply.", type = "warning")
      return()
    }
    
    # Get the dates identified by the scenario
    scenario_exclusion_dates <- as.Date(results$exclusions_df$date)
    
    # Combine with any existing exclusions already in rv$data
    current_excluded_status <- rv$data$excluded
    new_excluded_status <- current_excluded_status | (as.Date(rv$data$date) %in% scenario_exclusion_dates)
    
    # Apply the combined exclusions to rv$data
    rv$data$excluded <- new_excluded_status
    
    # Update the separate list of excluded dates (rv$excluded_dates)
    # Convert existing character dates back to Date, combine, get unique, convert back to character
    existing_dates_obj <- if(length(rv$excluded_dates) > 0) as.Date(rv$excluded_dates) else as.Date(character())
    combined_dates_obj <- unique(c(existing_dates_obj, scenario_exclusion_dates))
    rv$excluded_dates <- format(combined_dates_obj, "%Y-%m-%d")
    
    num_newly_applied <- sum(new_excluded_status) - sum(current_excluded_status)
    showNotification(paste("Applied", nrow(results$exclusions_df), "scenario exclusions.",
                           num_newly_applied, "additional data points marked."),
                     type = "message", duration = 8)
    
    # Disable the button to prevent applying the same scenario again
    shinyjs::disable("apply_scenario")
    
    # Suggest recalculation
    showNotification("Scenario exclusions applied. Click 'Recalculate Design Value' on the 'Exclude Dates' tab.", 
                     type = "message",
                     duration = 10)
    
    # Optionally, trigger recalculation automatically
    # shinyjs::click("recalculate") # If using shinyjs
    
    save_exclusion_state()
  })
  
  # Add this helper function inside server, before the last closing brace
  save_exclusion_state <- function() {
    if (!is.null(rv$data)) {
      state <- list(
        excluded = rv$data$excluded,
        excluded_dates = rv$excluded_dates,
        timestamp = Sys.time()
      )
      # Remove any states after current index (for new branch after undo)
      if (rv$current_state_index < length(rv$exclusion_states)) {
        rv$exclusion_states <- rv$exclusion_states[1:rv$current_state_index]
      }
      rv$exclusion_states <- append(rv$exclusion_states, list(state))
      rv$current_state_index <- length(rv$exclusion_states)
    }
  }
  
  })  # end moduleServer inner function
}     # end dvServer

# ============================================================
# Module UI (bslib)
# ============================================================
dvUI <- function(id) {
  ns <- NS(id)
  this_year <- as.integer(format(Sys.Date(), "%Y"))

  layout_sidebar(
    sidebar = sidebar(
      width = 300, title = "Site & Design-Value Period",
      uiOutput(ns("state_selector")),
      uiOutput(ns("county_selector")),
      uiOutput(ns("site_selector")),
      selectInput(ns("method_code"), "Primary Instrument Method:",
        choices = c(
          "Teledyne T640  (736↔636 @ 2024)"  = "T640_TRANSITION",
          "Teledyne T640x (738↔638 @ 2024)"  = "T640X_TRANSITION",
          "R&P PM2.5 FEM Mass (145)"               = "145",
          "Thermo PM2.5 FEM 5014i (188)"           = "188",
          "R&P PM2.5 FRM SA 2.2 (117)"             = "117"
        ), selected = "T640_TRANSITION"),
      helpText("Transition options auto-pick the correct method code per year."),
      numericInput(ns("dv_year"), "Design Value Year:", value = this_year,
                   min = 2000, max = this_year),
      actionButton(ns("fetch_data"), "Fetch Data",
                   icon = icon("cloud-arrow-down"), class = "btn-primary")
    ),

    navset_card_tab(
      id = ns("dv_tabs"),

      # ---- Overview ----
      nav_panel("Overview", icon = icon("table-cells"),
        layout_columns(col_widths = c(6, 6),
          uiOutput(ns("design_value_box")),
          uiOutput(ns("data_completeness_box"))
        ),
        card(full_screen = TRUE,
          card_header("PM2.5 Calendar Heatmaps (3-Year Period)"),
          layout_columns(col_widths = c(4, 4, 4),
            plotlyOutput(ns("calendar_heatmap_year1")),
            plotlyOutput(ns("calendar_heatmap_year2")),
            plotlyOutput(ns("calendar_heatmap_year3"))
          )
        ),
        card(full_screen = TRUE,
          card_header("Annual Means Trend"),
          plotOutput(ns("annual_means_plot"))
        ),
        card(full_screen = TRUE,
          card_header("Raw Data (3-Year Period)"),
          DTOutput(ns("data_table"))
        ),
        downloadButton(ns("download_analysis_report"),
                       "Download Complete Analysis Report", icon = icon("file-csv"))
      ),

      # ---- Exclude Dates ----
      nav_panel("Exclude Dates", icon = icon("calendar-minus"),
        layout_columns(col_widths = c(6, 6),
          card(
            card_header("Define Exclusions"),
            uiOutput(ns("date_range_inputs")),
            actionButton(ns("add_date_range"), "Add Date Range", icon = icon("plus")),
            hr(),
            uiOutput(ns("single_date_inputs")),
            actionButton(ns("add_single_date"), "Add Single Date", icon = icon("plus")),
            hr(),
            div(class = "d-flex flex-wrap gap-2",
              actionButton(ns("apply_exclusions"), "Apply Exclusions",
                           icon = icon("check"), class = "btn-primary"),
              actionButton(ns("reset"), "Reset", icon = icon("undo")),
              actionButton(ns("undo_exclusion"), "Undo", icon = icon("rotate-left")),
              actionButton(ns("redo_exclusion"), "Redo", icon = icon("rotate-right"))
            ),
            hr(),
            actionButton(ns("recalculate"), "Recalculate Design Value",
                         icon = icon("calculator"), class = "btn-success"),
            h5("Recalculated Design Value:", class = "mt-3"),
            verbatimTextOutput(ns("recalculated_design_value")),
            uiOutput(ns("dv_validation_summary"))
          ),
          card(
            card_header("Currently Excluded Dates"),
            DTOutput(ns("excluded_dates_table"))
          )
        )
      ),

      # ---- HMS Smoke ----
      nav_panel("HMS Smoke", icon = icon("fire"),
        card(
          card_header("High PM2.5 Days (> 15 µg/m³)"),
          helpText("Select a high day below to view NOAA HMS smoke polygons on the map."),
          DTOutput(ns("hms_high_days_table"))
        ),
        card(full_screen = TRUE,
          card_header("Smoke Polygon Map"),
          leafletOutput(ns("hms_smoke_map"), height = 600)
        )
      ),

      # ---- Event Log ----
      nav_panel("Event Log", icon = icon("list"),
        layout_columns(col_widths = c(5, 7),
          card(
            card_header("Add Event Documentation"),
            dateRangeInput(ns("event_date_range"), "Event Date Range:"),
            textInput(ns("event_type"), "Event Type (Wildfire, Dust Storm, ...):"),
            textInput(ns("aqs_flag"), "Suggested AQS Flag (RJ, R8, ...):"),
            numericInput(ns("exceedance_concentration"),
                         "Highest Concentration During Event (µg/m³):",
                         value = NA, min = 0),
            selectInput(ns("tier"), "Documentation Tier:",
                        choices = c("Tier 1", "Tier 2", "Tier 3", "N/A"), selected = "N/A"),
            textAreaInput(ns("notes"), "Notes / Justification:", rows = 4),
            actionButton(ns("add_event"), "Add Event to Log",
                         icon = icon("save"), class = "btn-primary")
          ),
          card(
            card_header("Event Log"),
            DTOutput(ns("event_log_table")),
            downloadButton(ns("download_event_log"), "Download Log")
          )
        )
      ),

      # ---- Scenario ----
      nav_panel("Scenario", icon = icon("chart-line"),
        card(
          card_header("Scenario Settings"),
          numericInput(ns("target_dv"), "Target Design Value:",
                       value = 9.0, step = 0.1, min = 0),
          checkboxGroupInput(ns("scenario_years"),
                             "Years for Potential Exclusion:", choices = NULL, inline = TRUE),
          actionButton(ns("run_scenario"), "Run Scenario Analysis",
                       icon = icon("play"), class = "btn-primary")
        ),
        card(
          card_header("Scenario Results"),
          verbatimTextOutput(ns("scenario_results")),
          hr(),
          h5("Dates Identified for Exclusion to Meet Target:"),
          DTOutput(ns("scenario_table")),
          hr(),
          actionButton(ns("apply_scenario"), "Apply Scenario Exclusions to Main Data",
                       icon = icon("check-double"))
        )
      )
    )
  )
}
