# Shiny_Smoke_Analysis_App_v4.R
# Enhanced Smoke & PM2.5 Analysis Dashboard
#
# Changes from v2:
#   - navbar_options() replaces deprecated bg/inverse in page_navbar()
#   - AQS credentials read from environment variables (no plaintext defaults)
#   - Hourly Cross-Section (Tab 2): timezone is now a user-controlled input, not hardcoded CST
#   - Tab 5 daily PM2.5 fetch now uses a ±5° bbox around the location (not all-US)
#   - map_vec() replaced with base-R do.call(c, lapply()) for purrr compatibility
#   - HRRR bbox uses named vector access ["xmin"] instead of [[1]]
#   - add_segments() threshold line converted to POSIXct to match x-axis type
#   - HRRR COLMD (column smoke) now extracted at monitor location and compared to
#     surface MASSDEN to estimate whether smoke is aloft or at surface (Tab 5)
#   - POC filter retained as-is per MDEQ workflow
#
# DO NOT MODIFY Shiny_Smoke_Analysis_App.R or Shiny_Smoke_Analysis_App_v2.R

# ============================================================
# ============================================================
# 1. PACKAGES
# ============================================================
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(lubridate)
  library(stringr)
  library(tidyr)
  library(scales)
  library(httr)
  library(readr)
  library(ggnewscale)
  library(purrr)
  library(cowplot)
  library(tools)
  library(tidygeocoder)
  library(maptiles)
  library(ggspatial)
  library(osmdata)
  library(ggrepel)
  library(tigris)
  library(maps)
  library(aqsr)
  library(stringi)
  library(shinycssloaders)
  library(leaflet)
  library(leaflet.extras2)
  library(plotly)
  library(DT)
  library(shinyWidgets)
  library(shinydashboard)
  library(shinyjs)
  library(splitr)
  library(furrr)
})

# Optional/system-dependent packages
if (requireNamespace("terra", quietly = TRUE)) suppressPackageStartupMessages(library(terra))

options(tigris_use_cache = TRUE, shiny.maxRequestSize = 50 * 1024^2)

# ============================================================
# 2. CREDENTIALS  (set env vars to avoid plaintext in source)
#    Windows: setx AQS_EMAIL "you@agency.gov"
AQS_EMAIL_DEFAULT <- Sys.getenv("AQS_EMAIL", "")
AQS_KEY_DEFAULT   <- Sys.getenv("AQS_KEY",   "")

# ============================================================
# 3. NATIONWIDE MSA DEFINITIONS
# ============================================================
msa_definitions <- list(
  "Albuquerque, NM"        = list(name="Albuquerque, NM MSA",           name_short="Albuquerque",  state_code="NM",            bbox_coords=c(xmin=-106.9,ymin=34.8,xmax=-106.3,ymax=35.4), gmt_offset=-7),
  "Atlanta, GA"            = list(name="Atlanta-Sandy Springs, GA MSA", name_short="Atlanta",      state_code="GA",            bbox_coords=c(xmin=-84.9,ymin=33.4,xmax=-84.0,ymax=34.2), gmt_offset=-5),
  "Austin, TX"             = list(name="Austin-Round Rock, TX MSA",     name_short="Austin",       state_code="TX",            bbox_coords=c(xmin=-98.0,ymin=30.1,xmax=-97.4,ymax=30.6), gmt_offset=-6),
  "Baltimore, MD"          = list(name="Baltimore-Columbia-Towson, MD", name_short="Baltimore",    state_code="MD",            bbox_coords=c(xmin=-77.0,ymin=39.0,xmax=-76.2,ymax=39.6), gmt_offset=-5),
  "Baton Rouge, LA"        = list(name="Baton Rouge, LA MSA",           name_short="BatonRouge",   state_code="LA",            bbox_coords=c(xmin=-91.5,ymin=30.1,xmax=-90.7,ymax=30.8), gmt_offset=-6),
  "Birmingham, AL"         = list(name="Birmingham-Hoover, AL MSA",     name_short="Birmingham",   state_code="AL",            bbox_coords=c(xmin=-87.4,ymin=33.2,xmax=-86.4,ymax=33.9), gmt_offset=-6),
  "Boston, MA"             = list(name="Boston-Cambridge, MA MSA",      name_short="Boston",       state_code="MA",            bbox_coords=c(xmin=-71.3,ymin=42.2,xmax=-70.8,ymax=42.6), gmt_offset=-5),
  "Charlotte, NC-SC"       = list(name="Charlotte-Concord, NC-SC MSA",  name_short="Charlotte",    state_code=c("NC","SC"),    bbox_coords=c(xmin=-81.2,ymin=35.0,xmax=-80.5,ymax=35.6), gmt_offset=-5),
  "Chattanooga, TN-GA"     = list(name="Chattanooga, TN-GA MSA",        name_short="Chattanooga",  state_code=c("TN","GA"),    bbox_coords=c(xmin=-85.5,ymin=34.8,xmax=-85.0,ymax=35.2), gmt_offset=-5),
  "Chicago, IL"            = list(name="Chicago-Naperville, IL-IN-WI",  name_short="Chicago",      state_code=c("IL","IN","WI"),bbox_coords=c(xmin=-88.3,ymin=41.5,xmax=-87.4,ymax=42.2), gmt_offset=-6),
  "Cincinnati, OH-KY-IN"   = list(name="Cincinnati, OH-KY-IN MSA",      name_short="Cincinnati",   state_code=c("OH","KY","IN"),bbox_coords=c(xmin=-84.8,ymin=38.8,xmax=-84.0,ymax=39.5), gmt_offset=-5),
  "Cleveland, OH"          = list(name="Cleveland-Elyria, OH MSA",      name_short="Cleveland",    state_code="OH",            bbox_coords=c(xmin=-82.1,ymin=41.2,xmax=-81.3,ymax=41.8), gmt_offset=-5),
  "Columbus, OH"           = list(name="Columbus, OH MSA",              name_short="Columbus",     state_code="OH",            bbox_coords=c(xmin=-83.3,ymin=39.8,xmax=-82.6,ymax=40.2), gmt_offset=-5),
  "Dallas-Fort Worth, TX"  = list(name="Dallas-Fort Worth, TX MSA",     name_short="DFW",          state_code="TX",            bbox_coords=c(xmin=-97.6,ymin=32.5,xmax=-96.5,ymax=33.3), gmt_offset=-6),
  "Denver, CO"             = list(name="Denver-Aurora, CO MSA",         name_short="Denver",       state_code="CO",            bbox_coords=c(xmin=-105.2,ymin=39.5,xmax=-104.5,ymax=40.0), gmt_offset=-7),
  "Detroit, MI"            = list(name="Detroit-Warren-Dearborn, MI",   name_short="Detroit",      state_code="MI",            bbox_coords=c(xmin=-83.4,ymin=42.1,xmax=-82.7,ymax=42.6), gmt_offset=-5),
  "El Paso, TX"            = list(name="El Paso, TX MSA",               name_short="ElPaso",       state_code="TX",            bbox_coords=c(xmin=-106.9,ymin=31.5,xmax=-106.2,ymax=32.0), gmt_offset=-7),
  "Gulfport-Biloxi, MS"    = list(name="Gulfport-Biloxi, MS MSA",       name_short="MSCoast",      state_code="MS",            bbox_coords=c(xmin=-89.7,ymin=30.0,xmax=-88.3,ymax=30.8), gmt_offset=-6),
  "Houston, TX"            = list(name="Houston-The Woodlands, TX MSA", name_short="Houston",      state_code="TX",            bbox_coords=c(xmin=-95.9,ymin=29.5,xmax=-95.0,ymax=30.1), gmt_offset=-6),
  "Huntsville, AL"         = list(name="Huntsville, AL MSA",            name_short="Huntsville",   state_code="AL",            bbox_coords=c(xmin=-86.8,ymin=34.4,xmax=-85.9,ymax=35.0), gmt_offset=-6),
  "Indianapolis, IN"       = list(name="Indianapolis-Carmel, IN MSA",   name_short="Indianapolis", state_code="IN",            bbox_coords=c(xmin=-86.5,ymin=39.5,xmax=-85.7,ymax=40.1), gmt_offset=-5),
  "Jackson, MS"            = list(name="Jackson, MS MSA",               name_short="JacksonMS",    state_code="MS",            target_aqsids=c("280490020","280490021"), bbox_coords=c(xmin=-90.5,ymin=32.0,xmax=-89.8,ymax=32.6), gmt_offset=-6),
  "Jacksonville, FL"       = list(name="Jacksonville, FL MSA",          name_short="Jacksonville", state_code="FL",            bbox_coords=c(xmin=-82.0,ymin=30.0,xmax=-81.3,ymax=30.6), gmt_offset=-5),
  "Kansas City, MO-KS"     = list(name="Kansas City, MO-KS MSA",        name_short="KansasCity",   state_code=c("MO","KS"),    bbox_coords=c(xmin=-94.8,ymin=38.8,xmax=-94.2,ymax=39.3), gmt_offset=-6),
  "Knoxville, TN"          = list(name="Knoxville, TN MSA",             name_short="Knoxville",    state_code="TN",            bbox_coords=c(xmin=-84.4,ymin=35.7,xmax=-83.6,ymax=36.3), gmt_offset=-5),
  "Las Vegas, NV"          = list(name="Las Vegas-Henderson, NV MSA",   name_short="LasVegas",     state_code="NV",            bbox_coords=c(xmin=-115.5,ymin=36.0,xmax=-114.8,ymax=36.5), gmt_offset=-8),
  "Little Rock, AR"        = list(name="Little Rock, AR MSA",           name_short="LittleRock",   state_code="AR",            bbox_coords=c(xmin=-92.7,ymin=34.5,xmax=-91.9,ymax=35.1), gmt_offset=-6),
  "Los Angeles, CA"        = list(name="Los Angeles-Long Beach, CA MSA",name_short="LosAngeles",   state_code="CA",            bbox_coords=c(xmin=-118.8,ymin=33.7,xmax=-117.8,ymax=34.4), gmt_offset=-8),
  "Memphis, TN-MS-AR"      = list(name="Memphis, TN-MS-AR MSA",         name_short="Memphis",      state_code=c("TN","MS","AR"),bbox_coords=c(xmin=-90.4,ymin=34.7,xmax=-89.6,ymax=35.5), gmt_offset=-6),
  "Miami, FL"              = list(name="Miami-Fort Lauderdale, FL MSA", name_short="Miami",        state_code="FL",            bbox_coords=c(xmin=-80.6,ymin=25.5,xmax=-80.0,ymax=26.3), gmt_offset=-5),
  "Milwaukee, WI"          = list(name="Milwaukee-Waukesha, WI MSA",    name_short="Milwaukee",    state_code="WI",            bbox_coords=c(xmin=-88.3,ymin=42.7,xmax=-87.7,ymax=43.2), gmt_offset=-6),
  "Minneapolis, MN-WI"     = list(name="Minneapolis-St. Paul, MN-WI",   name_short="Minneapolis",  state_code=c("MN","WI"),    bbox_coords=c(xmin=-93.7,ymin=44.7,xmax=-93.0,ymax=45.2), gmt_offset=-6),
  "Mobile, AL"             = list(name="Mobile, AL MSA",                name_short="Mobile",       state_code="AL",            bbox_coords=c(xmin=-88.5,ymin=30.5,xmax=-87.7,ymax=31.0), gmt_offset=-6),
  "Nashville, TN"          = list(name="Nashville-Davidson, TN MSA",    name_short="Nashville",    state_code="TN",            bbox_coords=c(xmin=-87.2,ymin=35.9,xmax=-86.4,ymax=36.6), gmt_offset=-6),
  "New Orleans, LA"        = list(name="New Orleans-Metairie, LA MSA",  name_short="NewOrleans",   state_code="LA",            bbox_coords=c(xmin=-90.5,ymin=29.7,xmax=-89.5,ymax=30.3), gmt_offset=-6),
  "New York, NY"           = list(name="New York-Newark, NY-NJ-CT MSA", name_short="NewYork",      state_code=c("NY","NJ","CT"),bbox_coords=c(xmin=-74.3,ymin=40.5,xmax=-73.7,ymax=41.0), gmt_offset=-5),
  "Oklahoma City, OK"      = list(name="Oklahoma City, OK MSA",         name_short="OKC",          state_code="OK",            bbox_coords=c(xmin=-97.8,ymin=35.3,xmax=-97.2,ymax=35.7), gmt_offset=-6),
  "Orlando, FL"            = list(name="Orlando-Kissimmee, FL MSA",     name_short="Orlando",      state_code="FL",            bbox_coords=c(xmin=-81.8,ymin=28.2,xmax=-81.0,ymax=28.9), gmt_offset=-5),
  "Philadelphia, PA-NJ"    = list(name="Philadelphia-Camden, PA-NJ MSA",name_short="Philadelphia", state_code=c("PA","NJ"),    bbox_coords=c(xmin=-75.4,ymin=39.8,xmax=-74.8,ymax=40.2), gmt_offset=-5),
  "Phoenix, AZ"            = list(name="Phoenix-Mesa, AZ MSA",          name_short="Phoenix",      state_code="AZ",            bbox_coords=c(xmin=-112.5,ymin=33.2,xmax=-111.7,ymax=33.8), gmt_offset=-7),
  "Pittsburgh, PA"         = list(name="Pittsburgh, PA MSA",            name_short="Pittsburgh",   state_code="PA",            bbox_coords=c(xmin=-80.2,ymin=40.2,xmax=-79.8,ymax=40.6), gmt_offset=-5),
  "Portland, OR-WA"        = list(name="Portland-Vancouver, OR-WA MSA", name_short="Portland",     state_code=c("OR","WA"),    bbox_coords=c(xmin=-122.9,ymin=45.3,xmax=-122.3,ymax=45.7), gmt_offset=-8),
  "Raleigh, NC"            = list(name="Raleigh-Cary, NC MSA",          name_short="Raleigh",      state_code="NC",            bbox_coords=c(xmin=-79.0,ymin=35.6,xmax=-78.4,ymax=36.1), gmt_offset=-5),
  "Richmond, VA"           = list(name="Richmond, VA MSA",              name_short="Richmond",     state_code="VA",            bbox_coords=c(xmin=-77.9,ymin=37.2,xmax=-77.2,ymax=37.7), gmt_offset=-5),
  "Sacramento, CA"         = list(name="Sacramento-Roseville, CA MSA",  name_short="Sacramento",   state_code="CA",            bbox_coords=c(xmin=-121.8,ymin=38.3,xmax=-121.0,ymax=38.8), gmt_offset=-8),
  "Salt Lake City, UT"     = list(name="Salt Lake City, UT MSA",        name_short="SaltLake",     state_code="UT",            bbox_coords=c(xmin=-112.2,ymin=40.4,xmax=-111.6,ymax=41.0), gmt_offset=-7),
  "San Antonio, TX"        = list(name="San Antonio, TX MSA",           name_short="SanAntonio",   state_code="TX",            bbox_coords=c(xmin=-98.8,ymin=29.2,xmax=-98.0,ymax=29.8), gmt_offset=-6),
  "San Diego, CA"          = list(name="San Diego-Carlsbad, CA MSA",    name_short="SanDiego",     state_code="CA",            bbox_coords=c(xmin=-117.6,ymin=32.5,xmax=-116.5,ymax=33.5), gmt_offset=-8),
  "San Francisco, CA"      = list(name="San Francisco-Oakland, CA MSA", name_short="SanFrancisco", state_code="CA",            bbox_coords=c(xmin=-122.6,ymin=37.5,xmax=-121.9,ymax=38.0), gmt_offset=-8),
  "Savannah, GA"           = list(name="Savannah, GA MSA",              name_short="Savannah",     state_code="GA",            bbox_coords=c(xmin=-81.3,ymin=31.9,xmax=-80.8,ymax=32.2), gmt_offset=-5),
  "Seattle, WA"            = list(name="Seattle-Tacoma-Bellevue, WA",   name_short="Seattle",      state_code="WA",            bbox_coords=c(xmin=-122.6,ymin=47.3,xmax=-121.9,ymax=47.8), gmt_offset=-8),
  "Shreveport, LA"         = list(name="Shreveport-Bossier City, LA",   name_short="Shreveport",   state_code="LA",            bbox_coords=c(xmin=-94.2,ymin=32.3,xmax=-93.5,ymax=32.8), gmt_offset=-6),
  "St. Louis, MO-IL"       = list(name="St. Louis, MO-IL MSA",          name_short="StLouis",      state_code=c("MO","IL"),    bbox_coords=c(xmin=-90.7,ymin=38.4,xmax=-90.0,ymax=38.9), gmt_offset=-6),
  "Tampa, FL"              = list(name="Tampa-St. Petersburg, FL MSA",  name_short="Tampa",        state_code="FL",            bbox_coords=c(xmin=-82.8,ymin=27.7,xmax=-82.1,ymax=28.2), gmt_offset=-5),
  "Tucson, AZ"             = list(name="Tucson, AZ MSA",                name_short="Tucson",       state_code="AZ",            bbox_coords=c(xmin=-111.1,ymin=32.1,xmax=-110.6,ymax=32.5), gmt_offset=-7),
  "Tulsa, OK"              = list(name="Tulsa, OK MSA",                 name_short="Tulsa",        state_code="OK",            bbox_coords=c(xmin=-96.2,ymin=35.9,xmax=-95.7,ymax=36.3), gmt_offset=-6),
  "Washington, DC"         = list(name="Washington-Arlington DC-VA-MD", name_short="WashDC",       state_code=c("DC","VA","MD"),bbox_coords=c(xmin=-77.5,ymin=38.6,xmax=-76.8,ymax=39.1), gmt_offset=-5),
  "-- Custom Location --"  = list(name="Custom Location",               name_short="Custom",       state_code=NULL,            bbox_coords=NULL, gmt_offset=-6)
)

# ============================================================
library(shiny)
library(dplyr)
library(readr)
library(sf)
library(maps)
library(ggplot2)
library(lubridate)
library(DT)
library(lwgeom)
library(httr)
library(png)
library(magick)
library(gridExtra)
library(grid)
library(mapdata)
library(splitr)
library(ggrepel)
library(purrr)
library(memoise)
library(foreach)
library(doParallel)
library(furrr)
library(future)
library(rvest)
library(stringr)
library(shinyjs)
library(openxlsx)
library(shinycssloaders)
library(promises)
library(leaflet)
library(tigris) 

# Global variables and functions
state_code_to_name <- c(
  "01" = "alabama", "02" = "alaska", "04" = "arizona", "05" = "arkansas", 
  "06" = "california", "08" = "colorado", "09" = "connecticut", "10" = "delaware", 
  "11" = "district of columbia", "12" = "florida", "13" = "georgia", "15" = "hawaii", 
  "16" = "idaho", "17" = "illinois", "18" = "indiana", "19" = "iowa", 
  "20" = "kansas", "21" = "kentucky", "22" = "louisiana", "23" = "maine", 
  "24" = "maryland", "25" = "massachusetts", "26" = "michigan", "27" = "minnesota", 
  "28" = "mississippi", "29" = "missouri", "30" = "montana", "31" = "nebraska", 
  "32" = "nevada", "33" = "new hampshire", "34" = "new jersey", "35" = "new mexico", 
  "36" = "new york", "37" = "north carolina", "38" = "north dakota", "39" = "ohio", 
  "40" = "oklahoma", "41" = "oregon", "42" = "pennsylvania", "44" = "rhode island", 
  "45" = "south carolina", "46" = "south dakota", "47" = "tennessee", "48" = "texas", 
  "49" = "utah", "50" = "vermont", "51" = "virginia", "53" = "washington", 
  "54" = "west virginia", "55" = "wisconsin", "56" = "wyoming", "84" = "wyoming"
)
state_name_to_code <- setNames(names(state_code_to_name), state_code_to_name)
# Normalize to Title Case for easier lookup
names(state_name_to_code) <- stringr::str_to_title(names(state_name_to_code))
# Specific fix for DC if needed (it's already in the list)


# Define concentration ranges and corresponding colors globally
colors_ozone <- c("green", "yellow", "orange", "red")
colors_pm25 <- c("green", "yellow", "orange", "red")
concentration_labels_ozone <- c("Good", "Moderate", "USG", "Unhealthy")
concentration_labels_pm25 <- c("Good", "Moderate", "USG", "Unhealthy")

state_agency_lookup <- c(
  "Alabama" = "Alabama Department of Environmental Management",
  "Alaska" = "Alaska Department of Environmental Conservation",
  "Arizona" = "Arizona Department of Environmental Quality",
  "Arkansas" = "Arkansas Department of Environmental Quality",
  "California" = "California Air Resources Board",
  "Colorado" = "Colorado Department of Public Health and Environment",
  "Connecticut" = "Connecticut Department of Energy and Environmental Protection",
  "Delaware" = "Delaware Department of Natural Resources and Environmental Control",
  "Florida" = "Florida Department of Environmental Protection",
  "Georgia" = "Georgia Environmental Protection Division",
  "Hawaii" = "Hawaii State Department of Health",
  "Idaho" = "Idaho Department of Environmental Quality",
  "Illinois" = "Illinois Environmental Protection Agency",
  "Indiana" = "Indiana Department of Environmental Management",
  "Iowa" = "Iowa Department of Natural Resources",
  "Kansas" = "Kansas Department of Health and Environment",
  "Kentucky" = "Kentucky Department for Environmental Protection",
  "Louisiana" = "Louisiana Department of Environmental Quality",
  "Maine" = "Maine Department of Environmental Protection",
  "Maryland" = "Maryland Department of the Environment",
  "Massachusetts" = "Massachusetts Department of Environmental Protection",
  "Michigan" = "Michigan Department of Environment, Great Lakes, and Energy",
  "Minnesota" = "Minnesota Pollution Control Agency",
  "Mississippi" = "Mississippi Department of Environmental Quality",
  "Missouri" = "Missouri Department of Natural Resources",
  "Montana" = "Montana Department of Environmental Quality",
  "Nebraska" = "Nebraska Department of Environment and Energy",
  "Nevada" = "Nevada Division of Environmental Protection",
  "New Hampshire" = "New Hampshire Department of Environmental Services",
  "New Jersey" = "New Jersey Department of Environmental Protection",
  "New Mexico" = "New Mexico Environment Department",
  "New York" = "New York State Department of Environmental Conservation",
  "North Carolina" = "North Carolina Department of Environmental Quality",
  "North Dakota" = "North Dakota Department of Environmental Quality",
  "Ohio" = "Ohio Environmental Protection Agency",
  "Oklahoma" = "Oklahoma Department of Environmental Quality",
  "Oregon" = "Oregon Department of Environmental Quality",
  "Pennsylvania" = "Pennsylvania Department of Environmental Protection",
  "Rhode Island" = "Rhode Island Department of Environmental Management",
  "South Carolina" = "South Carolina Department of Health and Environmental Control",
  "South Dakota" = "South Dakota Department of Environment and Natural Resources",
  "Tennessee" = "Tennessee Department of Environment and Conservation",
  "Texas" = "Texas Commission on Environmental Quality",
  "Utah" = "Utah Department of Environmental Quality",
  "Vermont" = "Vermont Department of Environmental Conservation",
  "Virginia" = "Virginia Department of Environmental Quality",
  "Washington" = "Washington State Department of Ecology",
  "West Virginia" = "West Virginia Department of Environmental Protection",
  "Wisconsin" = "Wisconsin Department of Natural Resources",
  "Wyoming" = "Wyoming Department of Environmental Quality"
)

## START of new code snippet ##
safe_download <- function(url, error_message = "Download failed") {
  tryCatch({
    response <- httr::GET(url, httr::timeout(60))
    cat(paste0("\n[", Sys.time(), "] Download URL: ", url, " - Status: ", httr::status_code(response), "\n"))
    
    if (httr::status_code(response) == 200) {
      return(list(success = TRUE, data = httr::content(response, "text", encoding = "UTF-8")))
    } else {
      return(list(success = FALSE, message = paste(error_message, "- Status:", httr::status_code(response))))
    }
  }, error = function(e) {
    cat(paste0("\n[", Sys.time(), "] Download Error for: ", url, " - Error: ", e$message, "\n"))
    return(list(success = FALSE, message = paste(error_message, "-", e$message)))
  })
}
## END of new code snippet ##

## START of new code snippet ##
# Create cache directory
cache_dir <- "cache"
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir)
}

# Cache function for expensive operations
cached_download <- function(url, cache_key, max_age_hours = 24) {
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
  
  if (file.exists(cache_file)) {
    cache_time <- file.info(cache_file)$mtime
    if (difftime(Sys.time(), cache_time, units = "hours") < max_age_hours) {
      return(readRDS(cache_file))
    }
  }
  
  result <- safe_download(url)
  if (result$success) {
    saveRDS(result, cache_file)
  }
  return(result)
}
## END of new code snippet ##

## START of new code snippet ##
validate_date_range <- function(start_date, end_date) {
  if (end_date < start_date) {
    return("End date must be after start date")
  }
  if (end_date > Sys.Date()) {
    return("End date cannot be in the future")
  }
  if (as.numeric(difftime(end_date, start_date, units = "days")) > 365) {
    return("Date range cannot exceed 365 days")
  }
  return(NULL)
}
## END of new code snippet ##

get_latest_tiering_csv_url <- function() {
  base_url <- "https://www.epa.gov/air-quality-analysis/pm25-tiering-tool-exceptional-events-analysis"
  backup_url <- "https://www.epa.gov/system/files/other-files/2025-10/for_posting.csv"
  
  tryCatch({
    # Read the webpage
    page <- read_html(base_url)
    
    # Find all links on the page
    links <- page %>% html_nodes("a") %>% html_attr("href")
    
    # Filter for CSV files related to tiering
    # Updated regex to catch standard naming or the specific backup name if linked
    csv_links <- links[str_detect(links, "r_fire_excluded_tiers.*\\.csv$|for_posting\\.csv$")]
    
    if (length(csv_links) == 0) {
      warning("No tiering CSV files found on the EPA page. Using backup.")
      return(backup_url)
    }
    
    # Get the most recent file (assuming the date in the filename is the most recent)
    # If no digits found (e.g. for_posting.csv), it will pick the first one found
    dates <- str_extract(csv_links, "\\d{8}")
    if(all(is.na(dates))) {
      latest_csv <- csv_links[1]
    } else {
      latest_csv <- csv_links[which.max(dates)]
    }
    
    # Construct the full URL
    if (startsWith(latest_csv, "http")) {
      full_url <- latest_csv
    } else if (startsWith(latest_csv, "/")) {
      full_url <- paste0("https://www.epa.gov", latest_csv)
    } else {
      full_url <- paste0("https://www.epa.gov/", latest_csv)
    }
    
    return(full_url)
  }, error = function(e) {
    warning(paste("Error fetching latest CSV URL:", e$message, "- Reverting to backup."))
    return(backup_url)
  })
}


# Function to clean geometry
clean_geometry <- function(geom) {
  tryCatch({
    if (sf::st_is_longlat(geom)) {
      geom <- sf::st_transform(geom, 3857)
    }
    if (any(sf::st_geometry_type(geom) == "GEOMETRYCOLLECTION")) {
      geom <- sf::st_collection_extract(geom, "POLYGON")
    }
    geom <- sf::st_cast(geom, "MULTIPOLYGON")
    geom <- sf::st_simplify(geom, dTolerance = 1e-8, preserveTopology = TRUE)
    geom <- sf::st_make_valid(geom)
    geom <- geom[sf::st_is_valid(geom),]
    geom <- sf::st_simplify(geom, dTolerance = 0.01, preserveTopology = TRUE)
    geom <- sf::st_buffer(geom, dist = 0.01)
    geom <- sf::st_buffer(geom, dist = -0.01)
    geom <- sf::st_make_valid(geom)
    geom <- sf::st_transform(geom, 4326)
    return(geom)
  }, error = function(e) {
    warning(paste("Error in clean_geometry:", e$message))
    return(sf::st_sfc(sf::st_multipolygon()))
  })
}

# Function to read KML data
read_kml <- function(date, layer, state_sf_transformed, sites_sf) {
  url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/KML/", 
                format(date, "%Y/%m/hms_smoke"), format(date, "%Y%m%d"), ".kml")
  
  tryCatch({
    kml_data <- sf::st_read(url, layer = layer, quiet = TRUE)
    kml_data_cleaned <- clean_geometry(kml_data)
    
    state_sf_transformed_local <- sf::st_transform(state_sf_transformed, sf::st_crs(kml_data_cleaned))
    state_sf_transformed_local <- clean_geometry(state_sf_transformed_local)
    
    state_smoke <- sf::st_intersection(kml_data_cleaned, state_sf_transformed_local)
    
    if (nrow(state_smoke) == 0) return(NULL)
    
    sites_sf_transformed <- sf::st_transform(sites_sf, sf::st_crs(state_smoke))
    
    # Convert to planar coordinate system for distance calculation
    state_smoke_planar <- sf::st_transform(state_smoke, 3857)
    sites_sf_planar <- sf::st_transform(sites_sf_transformed, 3857)
    
    # Find nearest smoke polygon for each site
    nearest_smoke <- st_nearest_feature(sites_sf_planar, state_smoke_planar)
    
    # Calculate distances
    distances <- st_distance(sites_sf_planar, state_smoke_planar[nearest_smoke,], by_element = TRUE)
    
    # Convert distances to numeric (meters) and filter
    max_distance <- 11000  # approximately 0.1 degrees
    sites_data <- sites_sf_transformed[as.numeric(distances) <= max_distance,]
    
    if (nrow(sites_data) == 0) return(NULL)
    
    sites_data$Smoke_Intensity <- layer
    sites_data$date <- as.Date(date)
    
    print(paste("Date:", date, "Layer:", layer, "Sites with smoke:", nrow(sites_data)))
    
    return(sites_data)
  }, error = function(e) {
    warning(paste("Failed to process layer", layer, "for date", date, ":", e$message))
    return(NULL)
  })
}

check_tiering_csv <- function(url) {
  tryCatch({
    # Read just the header to check columns
    data <- read.csv(url, nrows = 5)
    print(paste("Checking data structure from:", url))
    
    # Get lowercase column names for flexible matching
    cols <- tolower(names(data))
    
    # Check for SITE_ID (matches site_id, site.id, siteid)
    if (any(grepl("site.?id", cols))) {
      print("Column SITE_ID found.")
    } else {
      print("Column SITE_ID NOT found.")
    }
    
    # Check for Month
    if ("month" %in% cols) {
      print("Column month found.")
    } else {
      print("Column month NOT found.")
    }
    
    # Check for Tier 1 (matches tier.1, tier 1, tier1)
    if (any(grepl("tier.?1", cols))) {
      print("Column Tier 1 found.")
    } else {
      print("Column Tier 1 NOT found.")
    }
    
    # Check for Tier 2
    if (any(grepl("tier.?2", cols))) {
      print("Column Tier 2 found.")
    } else {
      print("Column Tier 2 NOT found.")
    }
    
    return(TRUE)
  }, error = function(e) {
    print(paste("Error checking tiering CSV:", e$message))
    # Return TRUE anyway to attempt processing in the main logic
    return(TRUE)
  })
}

# Function to generate combined plot for daily analysis
generate_combined_plot <- function(selectedDate, selectedState, ASOS_Stations, aqStateCode) {
  tryCatch({
    selected_year <- format(selectedDate, "%Y")
    
    base_url <- paste0("https://files.airnowtech.org/airnow/", selected_year, "/")
    formatted_date <- format(selectedDate, "%Y%m%d")
    daily_data_files <- readLines(paste0(base_url, formatted_date, "/daily_data_v2.dat"))
    
    # Use aqStateCode for air quality data filtering
    state_lines <- daily_data_files[grep(paste0("^", aqStateCode), daily_data_files)]
    
    if (length(state_lines) == 0) {
      return(list(error = "No data found for the selected state code and date"))
    }
    
    # Ozone data processing
    filtered_lines_ozone <- state_lines[grep("OZONE-8HR", state_lines)]
    data_ozone <- strsplit(filtered_lines_ozone, "\\|")
    lat_ozone <- as.numeric(sapply(data_ozone, function(x) x[11]))
    long_ozone <- as.numeric(sapply(data_ozone, function(x) x[12]))
    ozone_8hr <- as.numeric(sapply(data_ozone, function(x) x[6]))
    df_ozone <- data.frame(Latitude = lat_ozone, Longitude = long_ozone, Ozone_8HR = ozone_8hr)
    
    concentration_ranges_ozone <- c(0, 55, 71, 86, Inf)
    df_ozone$Concentration_Category <- cut(df_ozone$Ozone_8HR, breaks = concentration_ranges_ozone, labels = concentration_labels_ozone, right = FALSE)
    
    # PM2.5 data processing
    filtered_lines_pm25 <- state_lines[grep("PM2.5-24hr", state_lines)]
    data_pm25 <- strsplit(filtered_lines_pm25, "\\|")
    lat_pm25 <- as.numeric(sapply(data_pm25, function(x) x[11]))
    long_pm25 <- as.numeric(sapply(data_pm25, function(x) x[12]))
    pm25_24hr <- as.numeric(sapply(data_pm25, function(x) x[6]))
    df_pm25 <- data.frame(Latitude = lat_pm25, Longitude = long_pm25, PM25_24HR = pm25_24hr)
    
    concentration_ranges_pm25 <- c(0, 9, 35.4, 55.4, Inf)
    df_pm25$Concentration_Label <- cut(df_pm25$PM25_24HR, breaks = concentration_ranges_pm25, labels = concentration_labels_pm25, right = FALSE)
    
    # Meteorological data retrieval
    print("ASOS_Stations structure:")
    print(str(ASOS_Stations))
    
    ASOS_Stations <- ASOS_Stations %>%
      filter(!is.na(ICAO) & ICAO != "") %>%
      mutate(ICAO = gsub("^K", "", trimws(ICAO)))
    
    weather_data <- data.frame()
    
    for (icao in ASOS_Stations$ICAO) {
      url <- paste0("https://mesonet.agron.iastate.edu/cgi-bin/request/daily.py?network=", 
                    state.abb[which(state.name == selectedState)], "_ASOS&stations=", 
                    URLencode(as.character(icao)),
                    "&year1=", format(selectedDate, "%Y"), 
                    "&month1=", format(selectedDate, "%m"),
                    "&day1=", format(selectedDate, "%d"), 
                    "&year2=", format(selectedDate, "%Y"),
                    "&month2=", format(selectedDate, "%m"), 
                    "&day2=", format(selectedDate, "%d"))
      
      print(paste("Requesting data for station:", icao))
      print(paste("URL:", url))
      
      retry_count <- 0
      max_retries <- 1
      
      while (retry_count < max_retries) {
        tryCatch({
          response <- GET(url)
          if (status_code(response) == 200) {
            station_data <- read.csv(text = content(response, "text", encoding = "UTF-8"))
            if (nrow(station_data) > 0) {
              weather_data <- rbind(weather_data, station_data)
              print(paste("Successfully retrieved data for station:", icao))
              break  # Successful, exit the retry loop
            } else {
              print(paste("No data returned for station:", icao))
            }
          } else {
            print(paste("HTTP error for station", icao, "- Status code:", status_code(response)))
          }
          retry_count <- retry_count + 1
          if (retry_count == max_retries) {
            warning(paste("Failed to retrieve data for station", icao, "after", max_retries, "attempts"))
          }
        }, error = function(e) {
          print(paste("Error for station", icao, ":", conditionMessage(e)))
          retry_count <- retry_count + 1
        })
        Sys.sleep(1)  # Add a small delay between retries
      }
    }
    
    if (nrow(weather_data) == 0) {
      warning("No weather data could be retrieved for the selected date and state.")
      return(list(error = "No weather data available"))
    }
    
    weather_data <- weather_data %>% 
      rename(ICAO = station, ws = avg_wind_speed_kts, wd = avg_wind_drct, date = day) %>%
      mutate(across(c(wd, ws), ~as.numeric(as.character(.))))
    
    joined_data <- left_join(
      distinct(weather_data), 
      distinct(ASOS_Stations[, c("ICAO", "LAT", "LON")]), 
      by = "ICAO"
    )
    
    print("Joined data structure:")
    print(str(joined_data))
    
    numeric_cols <- c("max_temp_f", "min_temp_f", "max_dewpoint_f", "min_dewpoint_f", "precip_in", "min_rh")
    joined_data[numeric_cols] <- lapply(joined_data[numeric_cols], function(x) as.numeric(as.character(x)))
    joined_data$ws <- round(joined_data$ws, 1)
    
    # Get map data
    map_data_state <- map_data("state", region = tolower(selectedState))
    map_data_county <- map_data("county", region = tolower(selectedState))
    
    # Load state boundary data
    state_map <- map_data("state", region = tolower(selectedState))
    
    # Check which points are in state
    point_in_polygon <- function(point, poly) {
      point.x <- point[1]
      point.y <- point[2]
      poly.x <- poly$long
      poly.y <- poly$lat
      
      nvert <- length(poly.x)
      inside <- FALSE
      
      j <- nvert
      for (i in 1:nvert) {
        if (((poly.y[i] > point.y) != (poly.y[j] > point.y)) &&
            (point.x < (poly.x[j] - poly.x[i]) * (point.y - poly.y[i]) / (poly.y[j] - poly.y[i]) + poly.x[i])) {
          inside <- !inside
        }
        j <- i
      }
      
      return(inside)
    }
    
    points_in_state <- function(df, state_map) {
      sapply(1:nrow(df), function(i) {
        point_in_polygon(c(df$Longitude[i], df$Latitude[i]), state_map)
      })
    }
    
    df_ozone$in_state <- points_in_state(df_ozone, state_map)
    df_pm25$in_state <- points_in_state(df_pm25, state_map)
    
    # Generate plots
    map_ozone <- ggplot() +
      geom_polygon(data = state_map, aes(x = long, y = lat, group = group), fill = "#f9f9f9", color = "#333333", linewidth = 0.6) +
      geom_polygon(data = map_data("county", region = tolower(selectedState)), aes(x = long, y = lat, group = group), fill = NA, color = "#cccccc", linewidth = 0.2) +
      geom_point(data = df_ozone[df_ozone$in_state,], aes(x = Longitude, y = Latitude, color = Concentration_Category), size = 5, alpha = 0.8) +
      geom_text(data = df_ozone[df_ozone$in_state,], aes(x = Longitude, y = Latitude, label = Ozone_8HR), vjust = -0.8, size = 4.5, color = "#222222", fontface = "bold") +
      scale_color_manual(values = setNames(colors_ozone, levels(df_ozone$Concentration_Category))) +
      labs(title = paste(selectedState, "Ozone 8HR Concentrations"),
           subtitle = paste("Date:", format(selectedDate, "%Y-%m-%d")),
           color = "") +
      theme_void(base_size = 15) +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
            plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
            plot.margin = margin(10, 10, 10, 10)) +
      coord_fixed(ratio = 1.2)
    
    map_pm25 <- ggplot() +
      geom_polygon(data = state_map, aes(x = long, y = lat, group = group), fill = "#f9f9f9", color = "#333333", linewidth = 0.6) +
      geom_polygon(data = map_data("county", region = tolower(selectedState)), aes(x = long, y = lat, group = group), fill = NA, color = "#cccccc", linewidth = 0.2) +
      geom_point(data = df_pm25[df_pm25$in_state,], aes(x = Longitude, y = Latitude, color = Concentration_Label), size = 5, alpha = 0.8) +
      geom_text(data = df_pm25[df_pm25$in_state,], aes(x = Longitude, y = Latitude, label = PM25_24HR), vjust = -0.8, size = 4.5, color = "#222222", fontface = "bold") +
      scale_color_manual(values = setNames(colors_pm25, levels(df_pm25$Concentration_Label))) +
      labs(title = paste(selectedState, "PM2.5 24HR Concentrations"),
           subtitle = paste("Date:", format(selectedDate, "%Y-%m-%d")),
           color = "") +
      theme_void(base_size = 15) +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
            plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
            plot.margin = margin(10, 10, 10, 10)) +
      coord_fixed(ratio = 1.2)
    
    # Helper functions
    round_to_nearest <- function(x, base) {
      base * round(x / base)
    }
    
    get_nice_breaks <- function(min_val, max_val, n = 5) {
      range <- max_val - min_val
      if (range == 0) return(min_val)
      magnitude <- 10^floor(log10(range))
      step <- ceiling(range / (n * magnitude)) * magnitude
      seq(floor(min_val / step) * step, ceiling(max_val / step) * step, by = step)
    }
    
    # Generic plotting function for meteorological data
    plot_met_data <- function(joined_data, state_map, selectedState, selectedDate, var_name, plot_title, color_palette) {
      var_range <- range(joined_data[[var_name]], na.rm = TRUE)
      breaks <- get_nice_breaks(var_range[1], var_range[2])
      
      ggplot() +
        geom_polygon(data = state_map, aes(x = long, y = lat, group = group), fill = "#f9f9f9", color = "#333333", linewidth = 0.6) +
        geom_polygon(data = map_data("county", region = tolower(selectedState)), aes(x = long, y = lat, group = group), fill = NA, color = "#cccccc", linewidth = 0.2) +
        geom_point(data = joined_data, aes(x = LON, y = LAT, color = .data[[var_name]]), size = 4.5, alpha = 0.85) +
        geom_text_repel(data = joined_data, aes(x = LON, y = LAT, label = sprintf("%.1f", .data[[var_name]])), 
                        size = 3.8, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines"), fontface = "bold", color = "#222222") +
        scale_color_gradientn(
          name = plot_title, 
          colors = color_palette,
          breaks = breaks,
          labels = sprintf("%.0f", breaks),
          limits = range(breaks),
          guide = guide_colorbar(barwidth = 15, barheight = 0.5, title.position = "top", title.hjust = 0.5)
        ) +
        labs(title = paste(selectedState, plot_title),
             subtitle = paste("Date:", format(selectedDate, "%Y-%m-%d")),
             caption = "Source: IEMCOW") +
        theme_void(base_size = 14) +
        theme(legend.position = "bottom",
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
              plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
              plot.caption = element_text(size = 9, color = "#888888", hjust = 1),
              plot.margin = margin(10, 10, 10, 10)) +
        coord_fixed(ratio = 1.2)
    }
    
    # Define plot parameters for each meteorological variable
    met_plot_params <- list(
      max_temp = list(var_name = "max_temp_f", plot_title = "Max Temperature (F)", 
                      color_palette = c("blue", "green", "yellow", "red")),
      min_temp = list(var_name = "min_temp_f", plot_title = "Min Temperature (F)", 
                      color_palette = c("darkblue", "blue", "lightblue", "white")),
      max_dewpoint = list(var_name = "max_dewpoint_f", plot_title = "Max Dewpoint (F)", 
                          color_palette = c("lightyellow", "yellow", "orange", "red")),
      min_dewpoint = list(var_name = "min_dewpoint_f", plot_title = "Min Dewpoint (F)", 
                          color_palette = c("lightyellow", "yellow", "orange", "red")),
      wind_speed = list(var_name = "ws", plot_title = "Wind Speed (knots)", 
                        color_palette = c("blue", "green", "yellow", "red"))
    )
    
    
    degrees_to_cardinal <- function(degrees) {
      directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                      "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
      index <- round((degrees + 11.25) / 22.5) %% 16 + 1
      return(directions[index])
    }
    
    joined_data$cardinal_direction <- degrees_to_cardinal(joined_data$wd)
    
    # Function to generate all meteorological plots
    generate_met_plots <- function(joined_data, state_map, selectedState, selectedDate) {
      lapply(met_plot_params, function(params) {
        plot_met_data(joined_data, state_map, selectedState, selectedDate, 
                      params$var_name, params$plot_title, params$color_palette)
      })
    }
    
    # Wind direction plot (separate due to its unique nature)
    plot_wind_direction <- function(joined_data, state_map, selectedState, selectedDate) {
      ggplot() +
        geom_polygon(data = state_map, aes(x = long, y = lat, group = group), fill = "#f9f9f9", color = "#333333", linewidth = 0.6) +
        geom_polygon(data = map_data("county", region = tolower(selectedState)), aes(x = long, y = lat, group = group), fill = NA, color = "#cccccc", linewidth = 0.2) +
        geom_point(data = joined_data, aes(x = LON, y = LAT, color = cardinal_direction), size = 4.5, alpha = 0.85) +
        geom_text_repel(data = joined_data, aes(x = LON, y = LAT, label = cardinal_direction), 
                        size = 3.8, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines"), fontface = "bold", color = "#222222") +
        scale_color_manual(
          name = "Wind Direction",
          values = c("N" = "blue", "NNE" = "cyan", "NE" = "deepskyblue", "ENE" = "dodgerblue",
                     "E" = "green", "ESE" = "lightgreen", "SE" = "yellowgreen", "SSE" = "gold",
                     "S" = "orange", "SSW" = "darkorange", "SW" = "orangered", "WSW" = "red",
                     "W" = "darkred", "WNW" = "firebrick", "NW" = "maroon", "NNW" = "purple")
        ) +
        labs(title = paste(selectedState, "Wind Directions"),
             subtitle = paste("Date:", format(selectedDate, "%Y-%m-%d")),
             caption = "Source: IEMCOW") +
        theme_void(base_size = 14) +
        theme(legend.position = "bottom",
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
              plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
              plot.caption = element_text(size = 9, color = "#888888", hjust = 1),
              plot.margin = margin(10, 10, 10, 10)) +
        coord_fixed(ratio = 1.2) +
        guides(color = guide_legend(nrow = 2, byrow = TRUE, title.position = "top", title.hjust = 0.5, override.aes = list(size = 5)))
    }
    
    # Generate weather map image URLs
    weather_map_date <- as.Date(selectedDate) + 1
    weather_map_year <- format(weather_map_date, "%Y")
    weather_map_url <- paste0("https://www.wpc.ncep.noaa.gov/archives/sfc/", weather_map_year, "/namussfc", format(weather_map_date, "%Y%m%d00"), ".gif")
    
    # Generate upper air map image URLs
    date_format <- format(weather_map_date, "%y%m%d")
    upper_air_levels <- c("925", "850", "700", "500", "300")
    upper_air_urls <- sapply(upper_air_levels, function(level) {
      paste0("https://www.spc.noaa.gov/obswx/maps/", level, "_", date_format, "_00.gif")
    })
    
    # Download and process all maps
    download_and_process_map <- function(url) {
      temp_file <- tempfile(fileext = ".gif")
      download.file(url = url, destfile = temp_file, mode = "wb", quiet = TRUE)
      map <- tryCatch({
        magick::image_read(temp_file)
      }, error = function(e) {
        # If an error occurs, return an error message
        return(list(error = paste("An error occurred:", conditionMessage(e))))
      })
      if (!is.null(map)) {
        grid::rasterGrob(as.raster(map), interpolate = TRUE)
      } else {
        NULL
      }
    }
    
    weather_map_grob <- download_and_process_map(weather_map_url)
    upper_air_map_grobs <- lapply(upper_air_urls, download_and_process_map)
    
    # Generate meteorological plots
    met_plots <- generate_met_plots(joined_data, state_map, selectedState, selectedDate)
    wind_direction_plot <- plot_wind_direction(joined_data, state_map, selectedState, selectedDate)
    
    # Combine all plots
    air_quality_plot <- arrangeGrob(
      map_ozone, map_pm25, met_plots$max_temp,
      met_plots$min_temp, met_plots$max_dewpoint, met_plots$min_dewpoint,
      met_plots$wind_speed, wind_direction_plot, 
      ggplot() + theme_void() + ggtitle("Reserved for future use"),
      ncol = 3,
      widths = c(1, 1, 1),
      heights = c(1, 1, 1)
    )
    
    # Increase the size of the plot
    air_quality_plot <- ggplotGrob(ggplot() + annotation_custom(air_quality_plot) + 
                                     theme(plot.margin = margin(20, 20, 20, 20, "pt")))
    
    # Return all plots and maps
    return(list(
      air_quality_plot = air_quality_plot, 
      weather_map_plot = weather_map_grob,
      upper_air_925_plot = upper_air_map_grobs[[1]],
      upper_air_850_plot = upper_air_map_grobs[[2]],
      upper_air_700_plot = upper_air_map_grobs[[3]],
      upper_air_500_plot = upper_air_map_grobs[[4]],
      upper_air_300_plot = upper_air_map_grobs[[5]]
    ))
  }, error = function(e) {
    # If an error occurs, return an error message
    return(list(error = paste("An error occurred:", conditionMessage(e))))
  })
}

# Function to parse HMS Smoke data
parse_hms_smoke_data <- function(selectedDate, selectedState) {
  tryCatch({
    formatted_date <- format(selectedDate, "%Y%m%d")
    year <- format(selectedDate, "%Y")
    month <- format(selectedDate, "%m")
    
    kml_url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/KML/", year, "/", month, "/hms_smoke", formatted_date, ".kml")
    
    temp_file <- tempfile(fileext = ".kml")
    # Use quiet = TRUE to suppress the download status messages
    download.file(kml_url, temp_file, mode = "wb", quiet = TRUE)
    
    layers <- st_layers(temp_file)
    
    # If there are no layers in the KML file, return NULL early
    if (length(layers$name) == 0) {
      return(NULL)
    }
    
    smoke_data_list <- lapply(layers$name, function(layer_name) {
      tryCatch({
        # Suppress warnings during the read process for non-critical issues
        suppressWarnings({
          layer_data <- st_read(temp_file, layer = layer_name, quiet = TRUE)
        })
        # If the layer is empty or invalid, return NULL
        if (is.null(layer_data) || nrow(layer_data) == 0) return(NULL)
        layer_data$Density <- sub("Smoke \\((.+)\\)", "\\1", layer_name)
        return(layer_data)
      }, error = function(e) {
        warning(paste("Error reading KML layer:", layer_name, "-", conditionMessage(e)))
        return(NULL)
      })
    })
    
    # Filter out any NULLs that were returned from failed layer reads
    smoke_data_list <- Filter(Negate(is.null), smoke_data_list)
    
    # If the list is empty after filtering, it means no valid smoke layers were found
    if (length(smoke_data_list) == 0) {
      return(NULL)
    }
    
    smoke_data <- do.call(rbind, smoke_data_list)
    
    if (is.null(smoke_data) || nrow(smoke_data) == 0) {
      warning("No valid smoke data found in the KML file after combining layers.")
      return(NULL)
    }
    
    smoke_data <- st_zm(smoke_data, drop = TRUE, what = "ZM")
    
    smoke_data <- clean_geometry(smoke_data)
    
    if (nrow(smoke_data) == 0) {
      warning("All geometries were invalid and could not be fixed.")
      return(NULL)
    }
    
    national_smoke_data <- smoke_data
    
    # --- State-specific smoke data with improved checks ---
    state_smoke_data <- NULL # Initialize as NULL
    if (tolower(selectedState) != "national") {
      state_boundary <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
        filter(ID == tolower(selectedState))
      
      if(nrow(state_boundary) > 0) {
        state_boundary <- st_transform(state_boundary, st_crs(smoke_data))
        state_boundary <- clean_geometry(state_boundary)
        
        # Suppress the specific, expected warning about attribute variables
        suppressWarnings({
          state_smoke_data <- st_intersection(smoke_data, state_boundary)
        })
      }
    }
    
    # If state_smoke_data is still NULL or has 0 rows, create an empty sf object.
    # This ensures a consistent return type and avoids downstream errors.
    if (is.null(state_smoke_data) || nrow(state_smoke_data) == 0) {
      state_smoke_data <- st_as_sf(
        data.frame(Name = character(), Description = character(), Density = character(), geometry = st_sfc()),
        crs = st_crs(smoke_data)
      )
    }
    
    return(list(national = national_smoke_data, state = state_smoke_data))
    
  }, error = function(e) {
    warning(paste("Error in parse_hms_smoke_data:", conditionMessage(e)))
    return(NULL)
  })
}

# Function to generate HMS Smoke plots
generate_hms_smoke_plots <- function(selectedDate, selectedState, show_fires = TRUE) {
  tryCatch({
    smoke_data <- parse_hms_smoke_data(selectedDate, selectedState)
    
    if (is.null(smoke_data)) {
      return(list(
        state = ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid smoke data available") + theme_void(),
        national = ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid smoke data available") + theme_void()
      ))
    }
    
    # State plot
    state_plot <- ggplot() +
      geom_polygon(data = map_data("state", region = tolower(selectedState)), 
                   aes(x = long, y = lat, group = group), 
                   fill = "white", color = "black") +
      geom_sf(data = smoke_data$state, aes(fill = Density), alpha = 0.5) +
      scale_fill_manual(values = c("Light" = "lightblue", "Medium" = "grey", "Heavy" = "darkgrey")) +
      coord_sf() +
      labs(title = paste("HMS Smoke Data for", selectedState),
           subtitle = paste("Date:", format(selectedDate, "%Y-%m-%d")),
           fill = "Smoke Density") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Store state geometry for fire filtering
    state_coords <- map_data("state", region = tolower(selectedState))
    if (!is.null(state_coords) && nrow(state_coords) > 0) {
      smoke_data$state_geom <- st_as_sfc(st_bbox(st_as_sf(state_coords, coords = c("long", "lat"), crs = 4326)))
    } else {
      smoke_data$state_geom <- NULL
    }
    
    # National plot with state and province boundaries
    us_states <- map_data("state")
    canada_provinces <- map_data("worldHires", "Canada")
    
    national_plot <- ggplot() +
      geom_polygon(data = us_states, aes(x = long, y = lat, group = group), 
                   fill = "white", color = "darkgray", size = 0.2) +
      geom_polygon(data = canada_provinces, aes(x = long, y = lat, group = group), 
                   fill = "white", color = "darkgray", size = 0.2) +
      geom_sf(data = smoke_data$national, aes(fill = Density), alpha = 0.5) +
      geom_path(data = us_states, aes(x = long, y = lat, group = group), 
                color = "black", size = 0.3) +
      geom_path(data = canada_provinces, aes(x = long, y = lat, group = group), 
                color = "black", size = 0.3) +
      scale_fill_manual(values = c("Light" = "lightblue", "Medium" = "grey", "Heavy" = "darkgrey")) +
      coord_sf(xlim = c(-140, -50), ylim = c(25, 70), expand = FALSE) +
      labs(title = "HMS Smoke Data for US and Canada",
           subtitle = paste("Date:", format(selectedDate, "%Y-%m-%d")),
           fill = "Smoke Density") +
      theme_minimal() +
      theme(legend.position = "bottom",
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

    # Add fires if requested
    if (show_fires) {
      fires_sf <- fetch_hms_fires_day(selectedDate)
      if (!is.null(fires_sf) && nrow(fires_sf) > 0) {
        # State plot fire markers
        state_plot_fires <- if (!is.null(smoke_data$state_geom)) {
          fires_sf %>% st_filter(smoke_data$state_geom)
        } else {
          fires_sf
        }
        state_plot <- state_plot + 
          geom_sf(data = state_plot_fires, # Filter to state area
                  color = "red", size = 1.5, shape = 17)
        
        # National plot fire markers (subset to US/Canada)
        national_plot <- national_plot + 
          geom_sf(data = fires_sf, color = "red", size = 0.5, alpha = 0.6)
      }
    }
    
    return(list(state = state_plot, national = national_plot))
  }, error = function(e) {
    warning(paste("Error in generate_hms_smoke_plots:", conditionMessage(e)))
    return(list(
      state = ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                  label = paste("Error generating HMS Smoke plots:", conditionMessage(e))) + theme_void(),
      national = ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                     label = paste("Error generating HMS Smoke plots:", conditionMessage(e))) + theme_void()
    ))
  })
}

# New function to run trajectory model
run_trajectory_model <- function(lat, lon, heights, duration, start_date, daily_hours, direction, met_type) {
  tryCatch({
    # Use absolute paths
    project_dir <- normalizePath(".", winslash = "/")
    met_dir <- file.path(project_dir, "met")
    dir.create(met_dir, recursive = TRUE, showWarnings = FALSE)
    # Use a local temporary directory for execution to avoid OneDrive/Cloud sync issues
    exec_dir <- file.path(tempdir(), paste0("hysplit_run_", format(Sys.time(), "%H%M%S")))
    dir.create(exec_dir, recursive = TRUE, showWarnings = FALSE)
    
    # 1. Ensure met data is available (Manual HTTPS fallback)
    # This specifically addresses FTP blocks on government/OneDrive networks
    message("HYSPLIT Run: working directory is ", getwd())
    message("HYSPLIT Run: met_dir is ", met_dir)
    download_met_hysplit(start_date, met_type, met_dir)
    
    # Check if download succeeded
    filenames <- if (met_type == "reanalysis") {
        paste0("RP", format(as.Date(start_date), "%Y%m"), ".gbl")
      } else {
        mo_abbr <- tolower(format(as.Date(start_date), "%b"))
        yr_short <- format(as.Date(start_date), "%y")
        day_val <- as.integer(format(as.Date(start_date), "%d"))
        week_val <- ((day_val - 1) %/% 7) + 1
        if (week_val > 5) week_val <- 5
        paste0("gdas1.", mo_abbr, yr_short, ".w", week_val)
      }
    
    for (fn in filenames) {
      if (!file.exists(file.path(met_dir, fn))) {
        stop(paste("Meteorology file missing even after download attempt:", fn))
      } else {
        message("HYSPLIT Run: confirmed file exists: ", file.path(met_dir, fn), " (size: ", file.size(file.path(met_dir, fn)), ")")
      }
    }
    
    # 2. Get the correct HYSPLIT executable based on OS
    if (.Platform$OS.type == "unix") {
      # Mac/Linux
      hysplit_exe <- normalizePath(system.file("osx", "hyts_std", package = "splitr"), mustWork = FALSE)
      if (hysplit_exe == "" || !file.exists(hysplit_exe)) {
        hysplit_exe <- normalizePath(system.file("linux", "hyts_std", package = "splitr"), mustWork = FALSE)
      }
      if (file.exists(hysplit_exe)) Sys.chmod(hysplit_exe, "0755")
    } else {
      # Windows
      hysplit_exe <- normalizePath(system.file("win", "hyts_std.exe", package = "splitr"), winslash = "/", mustWork = FALSE)
    }
    
    # 3. Add HYSPLIT directory to PATH so splitr components can find it
    if (file.exists(hysplit_exe)) {
      Sys.setenv(PATH = paste(dirname(hysplit_exe), Sys.getenv("PATH"), sep = .Platform$path.sep))
    }
    
    # 4. Copy configuration files if they exist in project root
    for (cfg in c("ASCDATA.CFG", "SETUP.CFG", "ascdata.cfg", "setup.cfg")) {
      if (file.exists(file.path(project_dir, cfg))) {
        file.copy(file.path(project_dir, cfg), file.path(exec_dir, cfg), overwrite = TRUE)
      }
    }
    
    # 5. Create and run the trajectory model for each height
    results <- map(heights, function(height) {
      create_trajectory_model() %>% 
        add_trajectory_params(
          lat = lat,
          lon = lon,
          height = height,
          duration = duration,
          days = as.character(start_date),
          daily_hours = daily_hours,
          direction = direction,
          met_type = met_type,
          met_dir = met_dir,
          exec_dir = exec_dir
        ) %>% 
        run_model()
    })
    
    # Cleanup temp files (optional, but good practice)
    # unlink(exec_dir, recursive = TRUE)
    
    return(list(status = "success", data = results))
  }, error = function(e) {
    return(list(status = "error", message = conditionMessage(e)))
  })
}


# 4. HELPER FUNCTIONS
# ============================================================

get_olson_tz <- function(offset) {
  if (!is.numeric(offset) || is.na(offset)) return("GMT")
  if (offset == 0) return("GMT")
  if (offset < 0) return(paste0("Etc/GMT+", abs(offset)))
  paste0("Etc/GMT-", offset)
}

# Robust downloader for HYSPLIT met files (handles HTTPS fallbacks for FTP-blocked environments)
download_met_hysplit <- function(days, met_type, met_dir) {
  if (!dir.exists(met_dir)) dir.create(met_dir, recursive = TRUE)
  
  # Determine filenames ourselves (more robust than calling internal splitr functions)
  target_date <- as.Date(days)
  yr_full <- format(target_date, "%Y")
  yr_short <- format(target_date, "%y")
  mo_full <- format(target_date, "%m")
  mo_abbr <- tolower(format(target_date, "%b"))
  day_val <- as.integer(format(target_date, "%d"))
  
  filenames <- c()
  if (met_type == "reanalysis") {
    filenames <- paste0("RP", yr_full, mo_full, ".gbl")
  } else if (met_type == "gdas1") {
    # HYSPLIT Weekly Week calculation: 1-7 (w1), 8-14 (w2), 15-21 (w3), 22-28 (w4), 29+ (w5)
    week_val <- ((day_val - 1) %/% 7) + 1
    if (week_val > 5) week_val <- 5
    filenames <- paste0("gdas1.", mo_abbr, yr_short, ".w", week_val)
  }
  
  if (length(filenames) == 0) {
    message("HYSPLIT Downloader: Could not determine filenames for ", days)
    return(FALSE)
  }
  
  for (fn in filenames) {
    dest <- file.path(met_dir, fn)
    # If file doesn't exist or is suspiciously small (<1MB), try to download
    if (!file.exists(dest) || file.size(dest) < 1000000) {
      if (exists("incProgress")) incProgress(0.1, detail = paste("Downloading met:", fn))
      
      message("HYSPLIT Downloader: Attempting to fetch ", fn, " to ", met_dir)
      
      # Mirror list (HTTPS is much more stable than FTP in many environments)
      urls <- c(
        # Mirror 1: NOAA READY Data (Year subfolder)
        paste0("https://www.ready.noaa.gov/data/archives/", met_type, "/", yr_full, "/", fn),
        # Mirror 2: NOAA READY Data (Root folder)
        paste0("https://www.ready.noaa.gov/data/archives/", met_type, "/", fn),
        # Mirror 3: NOAA READY Pub (Archive subfolder)
        paste0("https://www.ready.noaa.gov/pub/archives/", met_type, "/", fn),
        # Mirror 4: AWS S3 PDS
        paste0("https://noaa-oar-arl-hysplit-pds.s3.amazonaws.com/", met_type, "/", yr_full, "/", mo_full, "/", fn)
      )
      
      success <- FALSE
      for (url in urls) {
        if (success) break
        message("HYSPLIT Downloader: Trying URL: ", url)
        try({
          # Use libcurl for best HTTPS compatibility on restricted networks
          download.file(url, dest, mode = "wb", quiet = FALSE, method = "libcurl")
          if (file.exists(dest) && file.size(dest) > 5000000) { # >5MB for a real met file
            success <- TRUE
            message("HYSPLIT Downloader: Successfully downloaded ", fn)
          } else {
            if (file.exists(dest)) unlink(dest)
          }
        }, silent = TRUE)
      }
      if (!success) {
        message("HYSPLIT Downloader: FAILED to download ", fn, " after trying all mirrors.")
      }
    } else {
      message("HYSPLIT Downloader: File already exists and is valid: ", fn)
    }
  }
  return(TRUE)
}

fetch_airnow_hourly <- function(start_date, end_date, states = NULL) {
  d1 <- min(as.Date(start_date), as.Date(end_date))
  d2 <- max(as.Date(start_date), as.Date(end_date))
  seq_h <- seq(as.POSIXct(paste(d1, "00:00:00"), tz = "GMT"),
               as.POSIXct(paste(d2, "23:59:59"), tz = "GMT"), by = "hour")
  all_df <- list()
  n <- length(seq_h)
  for (i in seq_along(seq_h)) {
    if (exists("incProgress")) incProgress(1/n, detail = paste("AirNow Hr", i, "of", n))
    dt <- seq_h[i]
    ymd_str <- format(dt, "%Y%m%d"); hr_str <- format(dt, "%H")
    url <- paste0("https://files.airnowtech.org/airnow/",
                  format(dt, "%Y"), "/", ymd_str, "/HourlyAQObs_", ymd_str, hr_str, ".dat")
    res <- try(GET(url), silent = TRUE)
    if (!inherits(res, "try-error") && status_code(res) == 200) {
      raw <- httr::content(res, "text", encoding = "UTF-8")
      if (trimws(raw) != "") {
        df <- suppressWarnings(read_csv(I(raw), quote = "\"", skip_empty_rows = TRUE,
          na = c("","NA"," ","N/A"), lazy = FALSE, show_col_types = FALSE,
          col_types = cols(.default = "?", AQSID = "c", PM25 = "d",
                           PM25_Measured = "i", ValidDate = "c", ValidTime = "c"),
          progress = FALSE))
        df <- df %>% filter(Status == "Active", PM25_Measured == 1, !is.na(PM25))
        if (!is.null(states)) df <- df %>% filter(StateName %in% states)
        if (nrow(df) > 0) {
          df$Hour_UTC <- as.integer(hr_str)
          all_df[[i]] <- df %>% select(AQSID, Latitude, Longitude, SiteName,
                                        PM25, ValidDate, Hour_UTC)
        }
      }
    }
  }
  bind_rows(all_df)
}

fetch_airnow_daily <- function(start_date, end_date, bbox = NULL, states = NULL) {
  d1 <- min(as.Date(start_date), as.Date(end_date))
  d2 <- max(as.Date(start_date), as.Date(end_date))
  seq_d <- seq(d1, d2, by = "day")
  all_df <- list()
  n <- length(seq_d)
  for (i in seq_along(seq_d)) {
    if (exists("incProgress")) incProgress(1/n, detail = paste("AirNow Daily", i, "of", n))
    d <- seq_d[i]
    ymd_str <- format(d, "%Y%m%d")
    url <- paste0("https://files.airnowtech.org/airnow/",
                  format(d, "%Y"), "/", ymd_str, "/daily_data_v2.dat")
    res <- try(httr::GET(url), silent = TRUE)
    if (!inherits(res, "try-error") && httr::status_code(res) == 200) {
      raw <- httr::content(res, "text", encoding = "UTF-8")
      if (trimws(raw) != "") {
        df <- suppressWarnings(read_delim(I(raw), delim = "|", col_names = FALSE, show_col_types = FALSE))
        if (nrow(df) > 0) {
          df <- df %>% filter(X4 == "PM2.5-24hr") %>%
            transmute(
              Monitoring_Site_ID = paste(substr(X2,1,2), substr(X2,3,5), substr(X2,6,9), sep="-"),
              StateName  = stringr::str_to_title(state_code_to_name[substr(X2,1,2)]),
              City       = X3,
              SiteName   = X3, # Standardize naming parity
              date       = d,
              Latitude   = as.numeric(X11),
              Longitude  = as.numeric(X12),
              pm25       = as.numeric(X6)
            ) %>%
            mutate(AQI_Cat = aqi_cat(pm25))
          
          if (!is.null(states)) {
            df <- df %>% filter(StateName %in% states)
          }
          
          if (!is.null(bbox)) {
            df <- df %>% filter(
              Longitude >= bbox["xmin"], Longitude <= bbox["xmax"],
              Latitude  >= bbox["ymin"], Latitude  <= bbox["ymax"]
            )
          }
          all_df[[i]] <- df
        }
      }
    }
  }
  if (length(all_df) == 0)
    return(data.frame(Monitoring_Site_ID=character(), City=character(), SiteName=character(), StateName=character(),
                      date=as.Date(character()), Latitude=numeric(),
                      Longitude=numeric(), pm25=numeric(), AQI_Cat=character()))
  bind_rows(all_df)
}

fetch_hms_smoke_day <- function(date) {
  d <- as.Date(date)
  url <- sprintf(
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/%s/%s/hms_smoke%s.zip",
    format(d, "%Y"), format(d, "%m"), format(d, "%Y%m%d"))
  tzip <- tempfile(fileext = ".zip"); tdir <- tempfile(pattern = "hms_smoke")
  dir.create(tdir, showWarnings = FALSE)
  on.exit(unlink(c(tzip, tdir), recursive = TRUE))
  if (inherits(try(download.file(url, tzip, mode = "wb", quiet = TRUE), silent = TRUE), "try-error"))
    return(NULL)
  unzip(tzip, exdir = tdir)
  shp <- list.files(tdir, pattern = "\\.shp$", full.names = TRUE)
  if (!length(shp)) return(NULL)
  sf_obj <- try(st_read(shp[1], quiet = TRUE), silent = TRUE)
  if (inherits(sf_obj, "try-error")) return(NULL)
  if (is.na(st_crs(sf_obj))) sf_obj <- st_set_crs(sf_obj, 4326)
  else if (st_crs(sf_obj)$epsg != 4326) sf_obj <- st_transform(sf_obj, 4326)
  sf_obj <- st_make_valid(sf_obj)
  if ("Density" %in% names(sf_obj)) sf_obj$Density <- str_to_title(sf_obj$Density)
  else if ("DENSITY" %in% names(sf_obj)) sf_obj$Density <- str_to_title(sf_obj$DENSITY)
  sf_obj
}

fetch_hms_fires_day <- function(date) {
  d <- as.Date(date)
  url <- sprintf(
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Shapefile/%s/%s/hms_fire%s.zip",
    format(d, "%Y"), format(d, "%m"), format(d, "%Y%m%d"))
  tzip <- tempfile(fileext = ".zip"); tdir <- tempfile(pattern = "hms_fire")
  dir.create(tdir, showWarnings = FALSE)
  on.exit(unlink(c(tzip, tdir), recursive = TRUE))
  if (inherits(try(download.file(url, tzip, mode = "wb", quiet = TRUE), silent = TRUE), "try-error"))
    return(NULL)
  unzip(tzip, exdir = tdir)
  shp <- list.files(tdir, pattern = "\\.shp$", full.names = TRUE)
  if (!length(shp)) return(NULL)
  fires <- try(st_read(shp[1], quiet = TRUE), silent = TRUE)
  if (inherits(fires, "try-error")) return(NULL)
  if (is.na(st_crs(fires))) fires <- st_set_crs(fires, 4326)
  else if (st_crs(fires)$epsg != 4326) fires <- st_transform(fires, 4326)
  fires
}

calc_fire_proximity <- function(fires_sf, point_sf, radius_km = 500) {
  if (is.null(fires_sf) || nrow(fires_sf) == 0)
    return(list(nearest_km = NA, nearest_bearing = NA,
                within_10 = 0, within_25 = 0, within_50 = 0,
                within_100 = 0, within_250 = 0, within_500 = 0, fires_df = NULL))
  fires_proj <- st_transform(fires_sf, 5070)
  point_proj <- st_transform(point_sf, 5070)
  dists_km   <- as.numeric(st_distance(point_proj, fires_proj)) / 1000
  nearest_i  <- which.min(dists_km)
  pt_c  <- st_coordinates(point_sf)
  fi_c  <- st_coordinates(st_transform(fires_sf[nearest_i, ], 4326))
  dx <- fi_c[1,1] - pt_c[1,1]; dy <- fi_c[1,2] - pt_c[1,2]
  bear_deg <- (atan2(dx, dy) * 180 / pi + 360) %% 360
  dirs <- c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
  compass <- dirs[floor(bear_deg / 22.5 + 0.5) %% 16 + 1]
  nearby  <- fires_sf[dists_km <= radius_km, ]
  nearby$dist_km <- round(dists_km[dists_km <= radius_km], 1)
  cols_keep <- intersect(c("dist_km","Lat","Lon","FRP","Satellite","Temp","DT","Start","End"), names(nearby))
  list(
    nearest_km      = round(min(dists_km), 1),
    nearest_bearing = compass,
    within_10   = sum(dists_km <= 10),
    within_25   = sum(dists_km <= 25),
    within_50   = sum(dists_km <= 50),
    within_100  = sum(dists_km <= 100),
    within_250  = sum(dists_km <= 250),
    within_500  = sum(dists_km <= 500),
    fires_df    = st_drop_geometry(nearby) %>% 
                    select(any_of(cols_keep)) %>%
                    group_by(dist_km, Lat, Lon, Satellite) %>%
                    summarise(
                      FRP = if("FRP" %in% names(.) && any(!is.na(FRP))) max(FRP, na.rm = TRUE) else NA_real_,
                      .groups = "drop"
                    ) %>%
                    arrange(dist_km, desc(FRP)) %>% 
                    head(25)
  )
}

# HRRR-Smoke via AWS S3 — fetches MASSDEN (surface, 8m) and COLMD (column-integrated)
fetch_hrrr_smoke <- function(date, hour = 12, bbox = NULL) {
  d <- as.Date(date)
  date_str <- format(d, "%Y%m%d"); hr_str <- sprintf("%02d", hour)
  idx_url  <- paste0("https://noaa-hrrr-bdp-pds.s3.amazonaws.com/hrrr.", date_str,
                     "/conus/hrrr.t", hr_str, "z.wrfsfcf00.grib2.idx")
  grib_url <- paste0("https://noaa-hrrr-bdp-pds.s3.amazonaws.com/hrrr.", date_str,
                     "/conus/hrrr.t", hr_str, "z.wrfsfcf00.grib2")

  if (!requireNamespace("terra", quietly = TRUE))
    return(list(surface = NULL, column = NULL,
                msg = "Install 'terra' to view HRRR-Smoke data."))

  idx_res <- try(httr::GET(idx_url, httr::timeout(10)), silent = TRUE)
  if (inherits(idx_res, "try-error") || httr::status_code(idx_res) != 200)
    return(list(surface = NULL, column = NULL,
                msg = paste0("HRRR archive not found for ", date_str, " ", hr_str,
                             "z. Data may be unavailable (archive covers ~48 days back).")))

  lines <- strsplit(httr::content(idx_res, "text", encoding = "UTF-8"), "\n")[[1]]

  fetch_layer <- function(search_str) {
    match_idx <- grep(search_str, lines)
    if (length(match_idx) == 0) return(NULL)
    idx   <- match_idx[1]
    b_start <- as.numeric(strsplit(lines[idx], ":")[[1]][2])
    b_end   <- if (idx < length(lines)) as.numeric(strsplit(lines[idx+1], ":")[[1]][2]) - 1 else ""
    grib_res <- try(httr::GET(grib_url,
      httr::add_headers(Range = paste0("bytes=", b_start, "-", b_end))), silent = TRUE)
    if (inherits(grib_res, "try-error") || !httr::status_code(grib_res) %in% c(200, 206))
      return(NULL)
    tmp <- tempfile(fileext = ".grib2")
    writeBin(httr::content(grib_res, "raw"), tmp)
    r <- try(terra::rast(tmp), silent = TRUE)
    if (inherits(r, "try-error")) return(NULL)
    if (!is.null(bbox)) {
      # FIX v3: use named vector access instead of [[1]] positional
      bb_sf <- st_bbox(c(
        xmin = as.numeric(bbox["xmin"]), ymin = as.numeric(bbox["ymin"]),
        xmax = as.numeric(bbox["xmax"]), ymax = as.numeric(bbox["ymax"])
      ), crs = 4326)
      poly    <- terra::vect(st_as_sfc(bb_sf))
      r_crop  <- try(terra::crop(r, terra::project(poly, terra::crs(r))), silent = TRUE)
      if (!inherits(r_crop, "try-error")) r <- r_crop
    }
    r
  }

  surf_rast <- fetch_layer(":MASSDEN:8 m above ground:")
  col_rast  <- fetch_layer(":COLMD:entire atmosphere")

  msg <- if (is.null(surf_rast) && is.null(col_rast))
    "MASSDEN & COLMD layers not found in GRIB index." else "OK"
  list(surface = surf_rast, column = col_rast, msg = msg)
}

# NEW v3: extract HRRR values at a point and determine smoke altitude verdict
assess_smoke_altitude <- function(hrrr_list, point_sf) {
  if (is.null(hrrr_list) || !requireNamespace("terra", quietly = TRUE))
    return(list(surface_ug = NA, column_mg = NA, verdict = "HRRR data unavailable."))

  extract_val <- function(rast_obj, pt, scale) {
    if (is.null(rast_obj)) return(NA_real_)
    pt_v <- try(terra::vect(st_transform(pt, terra::crs(rast_obj))), silent = TRUE)
    if (inherits(pt_v, "try-error")) return(NA_real_)
    val <- try(as.numeric(terra::extract(rast_obj, pt_v)[1, 2]), silent = TRUE)
    if (inherits(val, "try-error") || is.na(val)) return(NA_real_)
    val * scale
  }

  surf_ug <- extract_val(hrrr_list$surface, point_sf, 1e9)   # kg/m³ → µg/m³
  col_mg  <- extract_val(hrrr_list$column,  point_sf, 1e6)   # kg/m² → mg/m²

  verdict <- if (is.na(surf_ug) && is.na(col_mg)) {
    "HRRR values could not be extracted at this location."
  } else if (!is.na(col_mg) && !is.na(surf_ug)) {
    if      (col_mg >= 5  && surf_ug < 2)  "SMOKE ALOFT \u2014 Column smoke load is HIGH but surface concentration is LOW. Smoke is likely elevated above the mixing layer. Direct monitor impact is REDUCED."
    else if (col_mg >= 5  && surf_ug >= 2) "SMOKE AT SURFACE \u2014 Both column and surface concentrations are ELEVATED. Smoke is mixing down to the surface. Direct monitor impact is LIKELY."
    else if (col_mg < 5   && surf_ug >= 2) "NEAR-SURFACE SMOKE \u2014 Moderate surface smoke detected with low column load. Locally produced or shallow smoke layer."
    else                                    "LOW SMOKE \u2014 Both column and surface concentrations are low at this location per HRRR."
  } else if (!is.na(surf_ug)) {
    if (surf_ug >= 2) "Near-surface smoke detected (MASSDEN only \u2014 COLMD unavailable for altitude comparison)."
    else "Low near-surface smoke (MASSDEN only)."
  } else {
    "Column smoke data only (MASSDEN unavailable for surface comparison)."
  }

  list(
    surface_ug = if (is.na(surf_ug)) NA else round(surf_ug, 3),
    column_mg  = if (is.na(col_mg))  NA else round(col_mg,  3),
    verdict    = verdict
  )
}

# Qualitative smoke impact text (fire distance + HMS density + PM2.5)
smoke_impact_text <- function(nearest_km, within_100, density_levels, pm25_vals) {
  fire_context <- if (!is.na(nearest_km)) {
    if      (nearest_km < 100) "Fires are NEARBY (<100 km). Local/regional surface smoke likely."
    else if (nearest_km < 300) "Fires are REGIONAL (100\u2013300 km). Surface smoke impact probable."
    else if (nearest_km < 600) "Fires are DISTANT (300\u2013600 km). Smoke may be elevated/transported."
    else                        "Fires are FAR (>600 km). If smoke is present it is likely aloft."
  } else "No HMS fires found for this date."

  density_context <- if (!is.null(density_levels) && length(density_levels) > 0) {
    max_d <- if ("Heavy" %in% density_levels) "Heavy"
              else if ("Medium" %in% density_levels) "Medium"
              else "Light"
    paste0("Max HMS smoke density over location: ", max_d, ".")
  } else "No HMS smoke polygons intersected this location."

  pm25_context <- if (!is.null(pm25_vals) && length(pm25_vals) > 0) {
    mx <- max(pm25_vals, na.rm = TRUE)
    paste0("Observed max hourly PM2.5 nearby: ", round(mx, 1), " \u00b5g/m\u00b3 (",
           if (mx > 35.4) "ABOVE Moderate AQI threshold" else "within Moderate AQI range", ").")
  } else ""

  paste(fire_context, density_context, pm25_context, sep = "\n")
}

aqi_cat <- function(pm25) case_when(
  pm25 <= 9.0   ~ "Good",
  pm25 <= 35.4  ~ "Moderate",
  pm25 <= 55.4  ~ "Unhealthy for Sensitive Groups",
  pm25 <= 125.4 ~ "Unhealthy",
  pm25 <= 225.4 ~ "Very Unhealthy",
  TRUE          ~ "Hazardous"
)

aqi_pal <- colorFactor(
  palette = c("#00e400","#ffff00","#ff7e00","#ff0000","#8f3f97","#7e0023"),
  levels  = c("Good","Moderate","Unhealthy for Sensitive Groups",
              "Unhealthy","Very Unhealthy","Hazardous"),
  na.color = "gray"
)

smoke_fill_pal <- colorFactor(
  palette = c("#D3D3D3","#808080","#404040"),
  levels  = c("Light","Medium","Heavy"), na.color = "transparent"
)

# ============================================================
# 5. UI
# ============================================================
ui <- page_navbar(
  title = tags$span(tags$b("Air Quality & Smoke Analysis"), " \u2014 v4"),
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   primary = "#2c3e50", success = "#27ae60"),
  # FIX v3: navbar_options() replaces deprecated bg/inverse args
  navbar_options = navbar_options(bg = "#2c3e50", theme = "dark"),
  # Unified Header (CSS, head tags, and shinyjs)
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        .navbar-nav .nav-link { color: #ffffff !important; }
        .navbar-nav .nav-link.active { color: #f39c12 !important;
          border-bottom: 2px solid #f39c12; font-weight: bold; }
          
        /* Loading overlay */
        .loading-overlay {
          position: fixed; top: 0; left: 0; width: 100%; height: 100%;
          background: rgba(255, 255, 255, 0.9); z-index: 10000;
          display: flex; align-items: center; justify-content: center;
        }
        
        /* Progress indicators */
        .progress-info {
          background-color: #f8f9fa; border: 1px solid #dee2e6;
          border-radius: 5px; padding: 15px; margin: 10px 0;
        }
        
        /* Better button styling */
        .btn-primary { background-color: #007bff; border-color: #007bff; }
        .btn-primary:hover { background-color: #0056b3; border-color: #004085; }
        
        /* Improve tab appearance */
        .nav-tabs .nav-link { color: #495057; font-weight: 500; }
        .nav-tabs .nav-link.active { color: #007bff; border-bottom: 2px solid #007bff; }

        /* Better spacing for controls */
        .well { background-color: #f8f9fa; border: 1px solid #e9ecef; }
        .shiny-input-container { margin-bottom: 15px; }
      "))
    )
  ),
  
  nav_panel("AirNow MSA Analysis",
    layout_sidebar(
      sidebar = sidebar(width = 280,
        h6("Configuration"),
        dateRangeInput("msa_dates", "Local Date Range:",
                       start = Sys.Date() - 7, end = Sys.Date()),
        selectizeInput("msa_select", "Select MSA / Region:",
                       choices = names(msa_definitions),
                       options = list(placeholder = "Search MSAs...")),
        conditionalPanel(
          condition = "input.msa_select == '-- Custom Location --'",
          textInput("msa_custom_addr",   "Custom Address / City:", value = ""),
          numericInput("msa_custom_radius", "Radius (km):", value = 150, min = 10, max = 500),
          numericInput("msa_custom_gmt",    "GMT Offset (e.g. -6 for CST):", value = -6)
        ),
         radioButtons("msa_smoke_overlay", "HMS Smoke Overlay (Graph Shading):",
                      choices = c("Yes" = "y", "No" = "n"), selected = "y"),
         actionButton("run_msa", "Run Analysis", class = "btn-primary w-100 mt-2"),
        hr(),
        downloadButton("dl_msa_data",    "Download Raw CSV",     class = "btn-sm w-100 mb-1"),
        downloadButton("dl_msa_summary", "Download Summary CSV", class = "btn-sm w-100"),
        hr(),
        tags$div(class = "contact-info", style = "font-size: 0.85em; color: #666;",
                 "Questions, comments, or bug reports?",
                 tags$br(),
                 "Email Rodney Cuevas:",
                 tags$a("RCuevas@mdeq.ms.gov", href = "mailto:RCuevas@mdeq.ms.gov"))
      ),
      layout_columns(
        col_widths = 12,
        navset_card_underline(
          title = "PM2.5 Analysis Plots",
          nav_panel("Hourly Time Series", 
                    withSpinner(plotlyOutput("msa_ts_plot", height = "450px"))),
          nav_panel("Average Diurnal Profile", 
                    withSpinner(plotlyOutput("msa_diurnal_plot", height = "450px")))
        ),
        card(card_header("Site Summary Statistics"),
             withSpinner(DTOutput("msa_summary_tbl")))
      )
    )
  ),

  # ---- Tab 2: Hourly Smoke Cross-Section ----
  nav_panel("Hourly Cross-Section",
    layout_sidebar(
      sidebar = sidebar(width = 280,
        h6("Configuration"),
        dateInput("hourly_date", "Observation Date:", value = Sys.Date()),
        textInput("hourly_location", "Location Address:", value = "Hernando, MS"),
        numericInput("hourly_radius", "Search Radius (km):", value = 50, min = 1, max = 500),
        # FIX v3: timezone is now a user input, not hardcoded to CST
        numericInput("hourly_gmt_offset", "Local GMT Offset (e.g. -6 for CST, -5 for EST):",
                     value = -6, min = -12, max = 0, step = 1),
        checkboxInput("hourly_fires_toggle", "Display HMS Fires", value = TRUE),
        actionButton("run_hourly", "Generate Analysis", class = "btn-primary w-100 mt-2")
      ),
      layout_columns(
        col_widths = c(12, 6, 6),
        card(card_header("Hourly Smoke Density & PM2.5"),
             withSpinner(plotlyOutput("hourly_chart", height = "380px"))),
        card(card_header("Smoke Areal Map"),
             withSpinner(leafletOutput("hourly_map", height = "450px"))),
        card(card_header("Fire Proximity Summary"),
             withSpinner(verbatimTextOutput("hourly_fire_summary")),
             br(),
             withSpinner(DTOutput("hourly_fire_tbl")))
      )
    )
  ),

  # ---- Tab 3: Multi-Day AQS Smoke Events ----
  nav_panel("Multi-Day Smoke Events",
    layout_sidebar(
      sidebar = sidebar(width = 280,
        h6("Configuration"),
        radioButtons("aqs_src", "Data Source:",
                     choices = c("AQS API" = "aqs", "AirNow S3" = "airnow"), selected = "aqs"),
        conditionalPanel("input.aqs_src == 'aqs'",
          # FIX v3: no plaintext credential defaults — reads from env vars
          textInput("aqs_email", "AQS Email:",
                    value = AQS_EMAIL_DEFAULT,
                    placeholder = "set AQS_EMAIL env var"),
          passwordInput("aqs_key", "AQS Key:",
                        value = AQS_KEY_DEFAULT)
        ),
        dateRangeInput("aqs_dates", "Date Range:",
                       start = Sys.Date() - 30, end = Sys.Date()),
        selectInput("aqs_state", "State:", 
                    choices = names(state_agency_lookup), 
                    selected = "Mississippi"),
        numericInput("aqs_thresh", "PM2.5 Threshold (\u00b5g/m\u00b3):", value = 15, min = 1),
        checkboxGroupInput("aqs_smoke_filt", "Smoke Intensity:",
                           choices = c("Light","Medium","Heavy"),
                           selected = c("Medium","Heavy")),
        sliderInput("aqs_buffer", "Proximity Buffer (km):", value = 11, min = 0, max = 50, step = 1),
        helpText("Includes monitors within this distance of a smoke plume. 11km matches the standard used in HMS smoke analysis."),
        actionButton("run_aqs", "Run Analysis", class = "btn-primary w-100 mt-2"),
        hr(),
        downloadButton("dl_aqs_events", "Download Events CSV", class = "btn-sm w-100")
      ),
      layout_columns(
        col_widths = 12,
        card(card_header("Comprehensive Smoke Impact Summary & Event Highlights"),
             withSpinner(plotlyOutput("aqs_summary_plot", height = "600px"))),
        card(card_header("Significant Smoke Events Table (Filtered)"),
             withSpinner(DTOutput("aqs_event_tbl")))
      )
    )
  ),

  # ---- Tab 4: Single Day PM2.5 Map ----
  nav_panel("Single Day PM2.5 Map",
    layout_sidebar(
      sidebar = sidebar(width = 280,
        h6("Configuration"),
        radioButtons("sd_src", "Data Source:",
                     choices = c("AQS API" = "aqs", "AirNow S3" = "airnow"), selected = "aqs"),
        conditionalPanel("input.sd_src == 'aqs'",
          textInput("sd_email", "AQS Email:",
                    value = AQS_EMAIL_DEFAULT,
                    placeholder = "set AQS_EMAIL env var"),
          passwordInput("sd_key", "AQS Key:",
                        value = AQS_KEY_DEFAULT)
        ),
        dateInput("sd_date", "Date:", value = Sys.Date()),
        selectInput("sd_state", "State:", 
                    choices = names(state_agency_lookup), 
                    selected = "Mississippi"),
        checkboxInput("sd_fires", "Overlay HMS Fires", value = TRUE),
        actionButton("run_sd", "Generate Map", class = "btn-primary w-100 mt-2"),
        hr(),
        uiOutput("sd_value_boxes")
      ),
      card(card_header(uiOutput("sd_map_title")),
           withSpinner(leafletOutput("sd_map", height = "620px")))
    )
  ),

  # ---- Tab 5: Smoke Layer Analysis ----
  nav_panel("Smoke Layer Analysis",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        h6("Smoke Layer & Source Analysis"),
        helpText("Estimates whether smoke is near-surface or elevated/aloft using HMS fire",
                 "proximity, HRRR-Smoke surface vs column comparison, and qualitative indicators."),
        hr(),
        dateInput("sl_date",     "Analysis Date:",              value = Sys.Date()),
        textInput("sl_location", "Monitor / Location Address:", value = "Jackson, MS"),
        numericInput("sl_radius",    "Fire Search Radius (km):",    value = 500, min = 50, max = 2000),
        sliderInput("sl_hrrr_hour", "HRRR-Smoke Hour (UTC 0-23):",
                    min = 0, max = 23, value = 18, step = 1),
        numericInput("sl_gmt_offset", "Local GMT Offset (e.g. -6 for CST):",
                     value = -6, min = -12, max = 0, step = 1),
        helpText(style = "font-size: 0.8em; color: #666; margin-bottom: 15px;",
                 tags$b("Analysis Pro-Tip:"), " Smoke typically reaches the surface most effectively during peak mixing. Selecting ", 
                 tags$b("18:00 to 22:00 UTC"), " often provides the best diagnostic of maximum near-surface impact."),
        actionButton("run_sl", "Analyze", class = "btn-primary w-100 mt-2"),
        hr(),
        h6("HYSPLIT Back-Trajectory"),
        helpText("HYSPLIT trajectories show where air arriving at your monitor originated",
                 " at different altitudes (500m, 1500m, 3000m AGL)."),
        uiOutput("hysplit_link_ui")
      ),
      layout_columns(
        col_widths = c(7, 5),
        layout_columns(
          col_widths = 12,
          card(card_header("Fire Locations & Distance Rings"),
               withSpinner(leafletOutput("sl_fire_map", height = "420px"))),
          card(card_header("HRRR-Smoke Near-Surface Concentration (recent dates only)"),
               withSpinner(plotlyOutput("sl_hrrr_plot", height = "450px")),
               verbatimTextOutput("sl_hrrr_msg"))
        ),
        layout_columns(
          col_widths = 12,
          # NEW v3: HRRR altitude verdict card
          card(card_header("HRRR Smoke Altitude Estimate"),
               withSpinner(uiOutput("sl_altitude_ui"))),
          card(card_header("Smoke Impact Assessment"),
               withSpinner(verbatimTextOutput("sl_assessment")),
               hr(),
               h6("Smoke Level Interpretation Guide"),
               tags$ul(
                 tags$li(tags$b("Fires < 100 km:"),   " Local smoke \u2014 high probability of surface-level impact"),
                 tags$li(tags$b("Fires 100\u2013300 km:"), " Regional transport \u2014 likely surface impact if winds align"),
                 tags$li(tags$b("Fires 300\u2013600 km:"), " Long-range transport \u2014 smoke may be aloft in mixing layer"),
                 tags$li(tags$b("Fires > 600 km:"),   " Distant transport \u2014 smoke typically elevated, less direct PM2.5 impact"),
                 tags$li(tags$b("HRRR COLMD high + MASSDEN low:"),  " Smoke aloft, not mixing to surface"),
                 tags$li(tags$b("HRRR COLMD high + MASSDEN high:"), " Smoke at surface, impacting monitor")
               )
          ),
          card(card_header("Nearest Fires Table"),
               withSpinner(DTOutput("sl_fire_tbl")))
        )
      )
    )
  ),

  # ---- Tab 6: Monitor Data Prep ----
  nav_panel("Monitor Data Prep",
    layout_sidebar(
      sidebar = sidebar(width = 280,
        h6("Process Monitoring Stations"),
        helpText("Converts raw monitoring_site_locations.dat to clean Active PM2.5 CSV."),
        fileInput("site_dat", "Upload .dat file:", accept = c(".dat",".txt")),
        actionButton("run_prep", "Process", class = "btn-success w-100 mt-2"),
        hr(),
        downloadButton("dl_monitor_csv", "Download CSV", class = "btn-sm w-100")
      ),
      card(card_header("Status"),
           verbatimTextOutput("prep_status"),
           br(),
           withSpinner(DTOutput("prep_preview")))
    )
  ),

    tabPanel("HMS Smoke PM2.5 Analysis",
             fluidRow(
               column(3,
                      wellPanel(
                        dateRangeInput("dateRange", "Date Range", start = Sys.Date() - 30, end = Sys.Date()),
                        actionButton("downloadData", "Download Airnow Data", icon("download")),
                        pickerInput("stateCode", "State Code", choices = NULL, options = list(`live-search` = TRUE)),
                        pickerInput("countyCode", "County Code", choices = NULL, options = list(`live-search` = TRUE)),
                        pickerInput("siteId", "Site ID", choices = NULL, options = list(`live-search` = TRUE)),
                        checkboxGroupButtons("smokeIntensity", "Smoke Intensity", 
                                             choices = c("Light", "Medium", "Heavy"),
                                             selected = c("Light", "Medium", "Heavy")),
                        actionButton("processHMSData", "Process HMS Smoke Data", icon("play")),
                        tags$div(class = "contact-info",
                                 "Questions, comments, or bug reports?",
                                 tags$br(),
                                 "Email Rodney Cuevas:",
                                 tags$a("RCuevas@mdeq.ms.gov", href = "mailto:RCuevas@mdeq.ms.gov")
                        )
                      )
               ),
               column(9,
                      tabBox(
                        width = 12,
                        tabPanel("PM2.5 Data", withSpinner(DTOutput("dataTable"))),
                        tabPanel("Combined Data", withSpinner(DTOutput("combinedDataTable"))),
                        tabPanel("HMS Plot with Tiering", 
                                 fluidRow(
                                   column(12, 
                                          pickerInput("selectedSiteNames", "Select SiteNames", 
                                                      choices = NULL, multiple = TRUE,
                                                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                          downloadButton("downloadHMSPlot", "Download Plot"),
                                          withSpinner(plotOutput("hmsPlot", height = "600px"))
                                   )
                                 )
                        ),
                        tabPanel("Filtered Data",
                                 fluidRow(
                                   column(3,
                                          sliderInput("pm25Threshold", "PM2.5 Threshold (μg/m³)", 
                                                      min = 0, max = 100, value = 15, step = 0.1),
                                          checkboxGroupButtons("filteredSmokeIntensity", "Smoke Intensity", 
                                                               choices = c("Light", "Medium", "Heavy"),
                                                               selected = c("Medium", "Heavy")),
                                          radioButtons("tierFilter", "Filter by Tier",
                                                       choices = c("None" = "none",
                                                                   "≥ Tier 2" = "tier2",
                                                                   "≥ Tier 1" = "tier1"),
                                                       selected = "none")
                                   ),
                                   column(9,
                                          withSpinner(DTOutput("filteredDataTable")),
                                          downloadButton("downloadFilteredData", "Download Filtered Data", icon("file-download"))
                                   )
                                 )
                        )
                      )
               )
             )
    ),
    
    # --- Original Tab 2 ---
    tabPanel("Daily AQ, Met, HMS Smoke, and Back Trajectory Analysis",
             fluidRow(
               column(2,
                      wellPanel(
                        id = "analysis_params",
                        h4("Analysis Parameters", icon("sliders-h")),
                        dateInput("date", "Select Date:", value = Sys.Date()),
                        pickerInput("state", "Select State:", choices = state.name, options = list(`live-search` = TRUE)),
                        checkboxInput("aqs_fires", "Include HMS Fires on Maps", value = TRUE),
                        actionButton("generate", "Generate Plots", icon("chart-line"), 
                                     class = "btn-primary btn-block"),
                        tags$div(class = "contact-info",
                                 "Questions, comments, or bug reports?",
                                 tags$br(),
                                 "Email Rodney Cuevas:",
                                 tags$a("RCuevas@mdeq.ms.gov", href = "mailto:RCuevas@mdeq.ms.gov")
                        )
                      )
               ),
               column(10,
                      tabsetPanel(
                        tabPanel("Air Quality and Meteorological Data", 
                                 withSpinner(plotOutput("aqPlot", height = "800px", width = "100%"))
                        ),
                        tabPanel("Weather Maps", 
                                 fluidRow(
                                   column(2,
                                          selectInput("mapLevel", "Select Map Level:",
                                                      choices = c("Surface", "925mb", "850mb", "700mb", "500mb", "300mb"))
                                   ),
                                   column(10,
                                          withSpinner(plotOutput("weatherMap", height = "800px", width = "100%"))
                                   )
                                 )
                        ),
                        tabPanel("HMS Smoke", 
                                 fluidRow(
                                   column(6, withSpinner(plotOutput("hmsSmokePlotState", height = "600px", width = "100%"))),
                                   column(6, withSpinner(plotOutput("hmsSmokePlotNational", height = "600px", width = "100%")))
                                 )
                        ),
                        tabPanel("Back Trajectory",
                                 fluidRow(
                                   column(3,
                                          wellPanel(
                                            h4("Back Trajectory Analysis Instructions"),
                                            p("1. Select a date for the trajectory analysis."),
                                            p("2. Choose a state and a specific site within that state."),
                                            p("3. Set the starting heights for the trajectories."),
                                            p("4. Adjust the duration and other parameters as needed."),
                                            p("5. Click 'Generate Trajectory & Smoke' to create the plots.")
                                          ),
                                          wellPanel(
                                            h4("Trajectory Parameters", icon("route")),
                                            dateInput("trajectoryDate", "Select Date:", value = Sys.Date()),
                                            pickerInput("trajectoryStateCode", "State", choices = NULL, options = list(`live-search` = TRUE)),
                                            pickerInput("trajectorySiteName", "SiteName", choices = NULL, options = list(`live-search` = TRUE)),
                                            numericInput("height1", "Height 1 (m)", value = 50),
                                            numericInput("height2", "Height 2 (m)", value = 100),
                                            numericInput("height3", "Height 3 (m)", value = 200),
                                            sliderInput("duration", "Duration (hours)", min = 1, max = 240, value = 24),
                                            checkboxGroupButtons(
                                              "daily_hours", "Daily Hours",
                                              choices = c(0:23), 
                                              selected = c(0),
                                              status = "primary",
                                              checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                              width = "100%",
                                              size = "sm"
                                            ),
                                            radioGroupButtons(
                                              "direction", "Direction",
                                              choices = c("backward", "forward"),
                                              status = "primary"
                                            ),
                                            selectInput("met_type", "Meteorology Type", 
                                                        choices = c("reanalysis", "gdas1"), 
                                                        selected = "reanalysis"),
                                            tags$style(HTML(".btn-generate-trajectory { font-size: 0.9em; white-space: normal; height: auto; }")),
                                            actionButton("generateTrajectory", "Generate Trajectory & Smoke", 
                                                         icon("play"), 
                                                         class = "btn-success btn-block btn-generate-trajectory")
                                          )
                                   ),
                                   column(9,
                                          htmlOutput("lastTrajectoryUpdate"),
                                          fluidRow(
                                            box(
                                              width = 12,
                                              title = "Trajectory Plot",
                                              status = "primary",
                                              solidHeader = TRUE,
                                              withSpinner(plotOutput("trajectoryPlot", height = "600px"))
                                            )
                                          ),
                                          fluidRow(
                                            style = "margin-top: 20px;",
                                            box(
                                              width = 12,
                                              title = "Cross Section Plot",
                                              status = "info",
                                              solidHeader = TRUE,
                                              withSpinner(plotOutput("crossSectionPlot", height = "300px"))
                                            )
                                          )
                                   )
                                 )
                        )
                      )
               )
             )
    ),
    
    ## --- Start of additions from Snippet #6 --- ##
    tabPanel("Data Summary",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Analysis Controls"),
                 dateRangeInput("summary_dates", "Date Range:", start = Sys.Date() - 30, end = Sys.Date()),
                 pickerInput("summary_state", "Select States:", choices = state.name, 
                             selected = c("Mississippi", "Alabama", "Louisiana", "Tennessee", "Arkansas"),
                             multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                 actionButton("summarize_btn", "Generate Summary Dashboard", class = "btn-primary w-100", icon = icon("table")),
                 hr(),
                 helpText("Note: This dashboard provides a high-level statistical overview of the selected date range and states.")
               ),
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(12,
                          h3("Data Summary Dashboard"),
                          # Cleaned up KPI Banner with responsive wrapping
                          layout_column_wrap(
                            width = 1/4, heights_equal = "row", fill = FALSE,
                            valueBoxOutput("totalRecords"),
                            valueBoxOutput("dateRangeBox"),
                            valueBoxOutput("statesCount"),
                            valueBoxOutput("missingValuesBox")
                          ),
                          fluidRow(
                            column(6,
                                   h4("Record Count by State"),
                                   leafletOutput("stateMap", height = "400px"),
                                   plotOutput("dailyCountPlot", height = "300px")
                            ),
                            column(6,
                                   h4("Data Quality Report"),
                                   DTOutput("dataQualityTable"),
                                   h4("PM2.5 Value Distribution"),
                                   plotOutput("pm25DistributionPlot", height = "200px"),
                                   
                                   h4("Count by AQI Category"),
                                   # This div creates a scrollable window for our plot
                                   div(style = "height: 450px; overflow-y: auto; border: 1px solid #ddd;",
                                       plotOutput("aqiCategoryPlot") 
                                   )
                            )
                          )
                   )
                 )
               )
             )
    ),
    
    tabPanel("Settings",
             fluidRow(
               column(4,
                      h4("User & AQS Configuration"),
                      wellPanel(
                        textInput("settings_aqs_email", "AQS Email:", value = Sys.getenv("AQS_EMAIL", "")),
                        passwordInput("settings_aqs_key", "AQS Key:", value = Sys.getenv("AQS_KEY", "")),
                        hr(),
                        checkboxInput("autoRefresh", "Enable Auto-Refresh", value = FALSE),
                        numericInput("maxDownloadDays", "Maximum Download Days", value = 30, min = 1, max = 365),
                        checkboxInput("cacheEnabled", "Enable Data Caching", value = TRUE),
                        actionButton("clearCache", "Clear Cache", icon = icon("trash")),
                        hr(),
                        downloadButton("exportSettings", "Export Settings"),
                        fileInput("importSettings", "Import Settings", accept = ".json")
                      )
               ),
               column(4,
                      h4("API Connectivity Status"),
                      wellPanel(
                        uiOutput("api_status_dashboard"),
                        hr(),
                        h5("Contact Information"),
                        tags$div(class = "contact-info",
                                 "Questions, comments, or bug reports?",
                                 tags$br(),
                                 "Email Rodney Cuevas:",
                                 tags$a("RCuevas@mdeq.ms.gov", href = "mailto:RCuevas@mdeq.ms.gov")
                        ),
                        hr(),
                        p(tags$small("Application Version: 4.0.0-PROD")),
                        p(tags$small("Core Engine: bslib + future/parallel"))
                      )
               )
             )
    ),
  nav_spacer(),
  nav_item(tags$small(class = "text-white opacity-75",
    "Data: AirNow, HMS, HRRR | MDEQ Exceptional Events Analysis | v4")),
  nav_item(tags$img(src = "MDEQ_Logo.png", height = "40px", style = "margin-bottom:5px;"))
)

# ============================================================
# 6. SERVER
# ============================================================
server <- function(input, output, session) {
  # Set a higher timeout value
  options(timeout = 600)  # Set timeout to 600 seconds (10 minutes)
  
  app_settings <- reactiveValues(
    auto_refresh = FALSE,
    refresh_interval = 3600,  # seconds
    max_download_days = 30,
    cache_enabled = TRUE
  )
  
  # Sync UI settings to reactive values
  observe({
    app_settings$auto_refresh <- input$autoRefresh
    app_settings$max_download_days <- input$maxDownloadDays
    app_settings$cache_enabled <- input$cacheEnabled
  })
  
  # Update environment variables from Settings tab AND sync to other UI inputs
  observeEvent(input$settings_aqs_email, { 
    Sys.setenv(AQS_EMAIL = input$settings_aqs_email)
    updateTextInput(session, "aqs_email", value = input$settings_aqs_email)
    updateTextInput(session, "sd_email",  value = input$settings_aqs_email)
  })
  observeEvent(input$settings_aqs_key, { 
    Sys.setenv(AQS_KEY = input$settings_aqs_key)
    updateTextInput(session, "aqs_key", value = input$settings_aqs_key)
    updateTextInput(session, "sd_key",  value = input$settings_aqs_key)
  })
  
  # Also sync in reverse (from analysis tabs to Settings)
  observeEvent(input$aqs_email, { updateTextInput(session, "settings_aqs_email", value = input$aqs_email) })
  observeEvent(input$aqs_key,   { updateTextInput(session, "settings_aqs_key", value = input$aqs_key) })
  observeEvent(input$sd_email,  { updateTextInput(session, "settings_aqs_email", value = input$sd_email) })
  observeEvent(input$sd_key,    { updateTextInput(session, "settings_aqs_key", value = input$sd_key) })

  output$api_status_dashboard <- renderUI({
    # 1. Check AQS Credentials
    has_aqs_email <- nchar(input$settings_aqs_email %||% Sys.getenv("AQS_EMAIL")) > 0
    has_aqs_key   <- nchar(input$settings_aqs_key   %||% Sys.getenv("AQS_KEY"))   > 0
    
    # 2. Check AirNow Reachability (Public S3)
    # We check if we can reach the main data index
    airnow_status <- tryCatch({
      res <- httr::HEAD("https://files.airnowtech.org/airnow/", httr::timeout(2))
      httr::status_code(res) == 200
    }, error = function(e) FALSE)
    
    status_tag <- function(label, success, missing_text = " (Missing)", success_text = " (Connected)") {
      color <- if(success) "#28a745" else "#dc3545"
      icon_name  <- if(success) "check-circle" else "times-circle"
      tags$div(style = paste0("color:", color, "; margin-bottom:5px; font-weight:bold;"),
               icon(icon_name), " ", label, if(success) success_text else missing_text)
    }
    
    tagList(
      status_tag("AQS Credentials", has_aqs_email && has_aqs_key),
      status_tag("AirNow Data Server", airnow_status, " (Unreachable)", " (Online)")
    )
  })

  output$systemInfo <- renderPrint({
    cat("R Version:   ", R.version.string, "\n")
    cat("Platform:    ", R.version$platform, "\n")
    cat("OS Type:     ", .Platform$OS.type, "\n")
    cat("Shiny Ver:   ", as.character(packageVersion("shiny")), "\n")
    cat("Local Time:  ", as.character(Sys.time()), "\n")
    cat("Timezone:    ", Sys.timezone(), "\n")
  })
  ## END of new code snippet ##
  
  # Add the validation function here
  validateTrajectoryInputs <- function(input) {
    if (length(input$daily_hours) == 0) {
      return("Please select at least one daily hour to run the trajectory model.")
    }
    return(NULL)
  }
  
  tiering_csv_url <- reactiveVal(NULL)
  update_timer <- reactiveTimer(24 * 60 * 60 * 1000)  # 24 hours in milliseconds
  
  
  
  # Add this function to safely get the latest URL
  # Inside Server Function
  safely_get_latest_url <- function() {
    backup_url <- "https://www.epa.gov/system/files/other-files/2025-10/for_posting.csv"
    
    tryCatch({
      url <- get_latest_tiering_csv_url()
      
      if (!is.null(url)) {
        print(paste("Found tiering CSV URL:", url))
        # We verify if we can actually READ it. If not, we switch to backup immediately.
        if (check_tiering_csv(url)) {
          return(url)
        } else {
          print(paste("Scraped URL failed check. Using backup:", backup_url))
          return(backup_url)
        }
      } else {
        print(paste("Could not find latest tiering CSV URL. Using backup:", backup_url))
        return(backup_url)
      }
    }, error = function(e) {
      warning("Error in safely_get_latest_url: ", conditionMessage(e))
      return(backup_url)
    })
  }
  
  # Add this near the top of your server function
  trajectoryGenerated <- reactiveVal(FALSE)
  
  # Add this near the top of your server function
  values <- reactiveValues(
    lastSelectedState = NULL,
    lastSelectedSiteName = NULL
  )
  
  # Add this function at the top of your server logic
  get_date_breaks <- function(date_range) {
    days_diff <- as.numeric(diff(date_range))
    
    if (days_diff <= 14) {
      list(breaks = "1 day", labels = "%Y-%m-%d", angle = 90)
    } else if (days_diff <= 60) {
      list(breaks = "1 week", labels = "%Y-%m-%d", angle = 90)
    } else if (days_diff <= 365) {
      list(breaks = "1 month", labels = "%Y-%m", angle = 45)
    } else {
      list(breaks = "3 months", labels = "%Y-%m", angle = 45)
    }
  }
  
  # Add the new fetch_asos_data function here
  fetch_asos_data <- function(state) {
    state_abbr <- state.abb[which(state.name == state)]
    url <- paste0("https://mesonet.agron.iastate.edu/sites/networks.php?network=", state_abbr, "_ASOS&format=csv&nohtml=on")
    
    tryCatch({
      asos_data <- read.csv(url, stringsAsFactors = FALSE)
      asos_data <- asos_data %>%
        select(stid, station_name, lat, lon) %>%
        rename(ICAO = stid, NAME = station_name, LAT = lat, LON = lon) %>%
        mutate(STATE = state,
               CTRY = "US")
      return(asos_data)
    }, error = function(e) {
      warning(paste("Error fetching ASOS data for", state, ":", conditionMessage(e)))
      return(NULL)
    })
  }
  
  # Load ASOS_Stations data
  ASOS_Stations <- reactive({
    req(input$state)
    withProgress(message = 'Fetching ASOS data...', value = 0, {
      fetch_asos_data(input$state)
    })
  })
  
  # Load ASOS_Stations data
  # Replace the ASOS_Stations reactive with this:
  ASOS_Stations <- reactive({
    req(input$state)
    withProgress(message = 'Fetching ASOS data...', value = 0, {
      fetch_asos_data(input$state)
    })
  })
  
  
  observe({
    update_timer()  # This will invalidate the observer every 24 hours
    
    new_url <- safely_get_latest_url()
    
    if (!is.null(new_url)) {
      current_url <- tiering_csv_url()
      if (is.null(current_url) || new_url != current_url) {
        tiering_csv_url(new_url)
        showNotification("Updated tiering data is available and will be used.", type = "message")
      }
    }
  })
  

  # Modified tier_data reactive to handle R's automatic space-to-dot conversion
  # Inside Server Function
  tier_data <- reactive({
    # 1. Determine URL
    if (is.null(tiering_csv_url())) {
      url <- safely_get_latest_url()
      tiering_csv_url(url)
    }
    backup_url <- "https://www.epa.gov/system/files/other-files/2025-10/for_posting.csv"
    
    # 2. Download Data (Primary or Backup)
    raw_data <- tryCatch({
      current_url <- tiering_csv_url()
      print(paste("Loading tiering data from:", current_url))
      read.csv(current_url, stringsAsFactors = FALSE)
    }, error = function(e) {
      print(paste("Primary URL failed:", e$message))
      print(paste("Attempting backup URL:", backup_url))
      showNotification("Primary tiering data unavailable. Using backup data.", type = "warning")
      tiering_csv_url(backup_url) # Update reactive to use backup next time
      read.csv(backup_url, stringsAsFactors = FALSE)
    })
    
    # 3. Process Data (Renaming Columns)
    # We perform this on 'raw_data' regardless of where it came from
    tryCatch({
      data <- raw_data
      print("Original Tier Data Column Names:")
      print(names(data))
      
      # Convert names to lowercase for easy matching
      current_names <- tolower(names(data))
      
      # Rename Tier 1 (Matches 'tier 1', 'tier.1', 'tier1')
      t1_idx <- grep("tier.?1", current_names)
      if (length(t1_idx) > 0) names(data)[t1_idx[1]] <- "Tier.1"
      
      # Rename Tier 2 (Matches 'tier 2', 'tier.2', 'tier2')
      t2_idx <- grep("tier.?2", current_names)
      if (length(t2_idx) > 0) names(data)[t2_idx[1]] <- "Tier.2"
      
      # Rename SITE_ID
      site_idx <- grep("site.?id", current_names)
      if (length(site_idx) > 0) names(data)[site_idx[1]] <- "SITE_ID"
      
      # Rename Month
      if ("month" %in% current_names) {
        names(data)[which(current_names == "month")[1]] <- "month"
      }
      
      print("Final Processed Column Names:")
      print(names(data))
      
      # Final Check
      if (!all(c("Tier.1", "Tier.2", "SITE_ID", "month") %in% names(data))) {
        print("CRITICAL: Missing required columns even after processing.")
        return(NULL)
      }
      
      # Return cleaned data
      data %>%
        mutate(
          SITE_ID = as.character(SITE_ID),
          month = as.integer(month),
          Tier.1 = as.numeric(Tier.1),
          Tier.2 = as.numeric(Tier.2)
        )
    }, error = function(e) {
      showNotification(paste("Error processing tiering columns:", e$message), type = "error")
      return(NULL)
    })
  })
  
  
  # Reactive values
  pm25Data <- reactiveVal(NULL)
  combinedData <- reactiveVal(NULL)
  state_sf_transformed <- reactiveVal(NULL)
  
  # Add the new observe block here, after the reactive values
  observe({
    req(pm25Data())
    state_codes <- unique(pm25Data()$State_Code)
    updatePickerInput(session, "aqStateCode", choices = state_codes)
  })
  
  
  # HMS Smoke PM2.5 Analysis Server Logic
  download_airnow_data_bulk <- function(start_date, end_date) {
    # Validate inputs first
    validation_msg <- validate_date_range(start_date, end_date)
    if (!is.null(validation_msg)) {
      stop(validation_msg)
    }
    
    date_sequence <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day")
    # Parallelism is handled by the future_promise wrapper
    
    urls <- sapply(date_sequence, function(date) {
      year <- format(date, "%Y")
      yyyymmdd <- format(date, "%Y%m%d")
      paste0("https://files.airnowtech.org/airnow/", year, "/", yyyymmdd, "/daily_data_v2.dat")
    })
    
    # Run the download in the background
    # Run the download sequentially inside the background promise for maximum cloud stability
    all_data <- purrr::map_dfr(urls, function(url) {
      result <- safe_download(url)
      if (result$success) {
        suppressMessages(suppressWarnings({
          tryCatch({
            readr::read_delim(
              I(result$data),
              delim = "|",
              col_names = c("Valid_date", "AQSID", "SiteName", "Parameter_name", "Reporting_units", "Value", "Averaging_period", "Data_Source", "AQI_Value", "AQI_Category", "Latitude", "Longitude", "Full_AQSID"),
              col_types = readr::cols(.default = col_character()),
              show_col_types = FALSE
            )
          }, error = function(e) { NULL })
        }))
      } else {
        NULL
      }
    })
    
    # Perform final data processing
    if (is.null(all_data) || nrow(all_data) == 0) {
      stop("No data was returned from AirNow. Please check the date range and your internet connection.")
    }
    
    all_data <- all_data %>%
      mutate(
        Valid_date = lubridate::mdy(Valid_date),
        Latitude = as.numeric(Latitude),
        Longitude = as.numeric(Longitude),
        Value = as.numeric(Value),
        State_Code = substr(AQSID, 1, 2),
        County_Code = substr(AQSID, 3, 5),
        Site_ID = substr(AQSID, 6, 9)
      ) %>%
      filter(Parameter_name == "PM2.5-24hr", !is.na(Value), !is.na(Valid_date))
    
    if (nrow(all_data) == 0) {
      stop("No valid PM2.5-24hr data was found after processing.")
    }
    
    return(all_data)
  }
  
  # Final, robust observer for the download button using promises
  # Final, robust observer for the download button using promises
  observeEvent(input$downloadData, {
    # 1. Disable button and show a modal for immediate feedback
    shinyjs::disable("downloadData")
    showModal(modalDialog("Downloading AirNow data... this may take a few moments.", 
                          title = "Processing...", 
                          footer = NULL))
    
    # 2. Read the reactive inputs into normal variables HERE, before the promise
    start_date_val <- input$dateRange[1]
    end_date_val <- input$dateRange[2]
    
    # 3. Start the long process in the background, passing the NORMAL VARIABLES
    future_promise({
      # This code runs in a separate R process and uses the variables, not the inputs
      download_airnow_data_bulk(start_date_val, end_date_val)
    }) %...>% (function(all_data) {
      # 4. 'then' -> This code runs ONLY if the promise succeeds
      showNotification(paste("Success! Loaded", nrow(all_data), "PM2.5 records."), 
                       type = "message", duration = 8)
      pm25Data(all_data)
      updatePickerInput(session, "stateCode", choices = c("ALL", sort(unique(all_data$State_Code))))
    }) %...!% (function(error) {
      # 5. 'catch' -> This code runs ONLY if the promise fails
      showNotification(paste("Error:", error$message), type = "error", duration = 10)
    }) %>% finally(~{
      # 6. 'finally' -> This code runs regardless of success or failure
      removeModal()
      shinyjs::enable("downloadData")
    })
  })
  
  # Usage in Shiny app
  # Event handler for downloading data
  # Reactive for trajectory AirNow data
  
  trajectoryAirNowData <- reactive({
    req(input$trajectoryDate)
    downloadTrajectoryAirNowData(input$trajectoryDate)
  })
  
  observe({
    req(pm25Data())
    tryCatch({
      state_code <- input$stateCode
      
      county_choices <- if(!is.null(state_code) && state_code != "") {
        if(state_code == "ALL") {
          c("ALL", sort(unique(pm25Data()$County_Code)))
        } else {
          c("ALL", sort(unique(pm25Data()[pm25Data()$State_Code == state_code, ]$County_Code)))
        }
      } else {
        c("ALL")
      }
      
      updatePickerInput(session, "countyCode", choices = county_choices)
    }, error = function(e) {
      warning(paste("Error in observe block:", e$message))
    })
  })
  
  observe({
    req(pm25Data(), input$stateCode, input$countyCode)
    state_code <- input$stateCode
    county_code <- input$countyCode
    
    site_choices <- if(state_code == "ALL" && county_code == "ALL") {
      c("ALL", sort(unique(pm25Data()$Site_ID)))
    } else if(state_code != "ALL" && county_code == "ALL") {
      c("ALL", sort(unique(pm25Data()[pm25Data()$State_Code == state_code, ]$Site_ID)))
    } else if(state_code != "ALL" && county_code != "ALL") {
      c("ALL", sort(unique(pm25Data()[pm25Data()$State_Code == state_code & pm25Data()$County_Code == county_code, ]$Site_ID)))
    } else {
      c("ALL")
    }
    
    updatePickerInput(session, "siteId", choices = site_choices)
  })
  
  observe({
    req(combinedData())
    updatePickerInput(session, "selectedSiteNames", 
                      choices = unique(combinedData()$SiteName),
                      selected = NULL)
  })
  
  # Filtered PM2.5 Data
  filteredPM25Data <- reactive({
    req(pm25Data())
    
    tryCatch({
      data <- pm25Data()
      
      # Add debugging output
      print("PM2.5 Data Structure:")
      str(data)
      print("Number of rows in PM2.5 Data:")
      print(nrow(data))
      print("Unique State Codes:")
      print(unique(data$State_Code))
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      if (!is.null(input$stateCode) && input$stateCode != "ALL") {
        data <- data[data$State_Code == input$stateCode, ]
      }
      if (!is.null(input$countyCode) && input$countyCode != "ALL") {
        data <- data[data$County_Code == input$countyCode, ]
      }
      if (!is.null(input$siteId) && input$siteId != "ALL") {
        data <- data[data$Site_ID == input$siteId, ]
      }
      
      # Add more debugging output
      print("Filtered Data Structure:")
      str(data)
      print("Number of rows in Filtered Data:")
      print(nrow(data))
      
      if (nrow(data) == 0) {
        return(NULL)
      }
      
      data
    }, error = function(e) {
      print(paste("Error in filteredPM25Data:", conditionMessage(e)))
      NULL
    })
  })
  
  # Render data tables
  output$dataTable <- renderDT({
    req(filteredPM25Data())
    filteredPM25Data() %>%
      mutate(Valid_date = format(Valid_date, "%Y-%m-%d")) %>%
      datatable(options = list(pageLength = 15, lengthMenu = c(15, 30, 50), scrollX = TRUE, autoWidth = TRUE)) %>%
      formatStyle(columns = names(filteredPM25Data()), width = '100%')
  })
  
  output$combinedDataTable <- renderDT({
    req(combinedData())
    combinedData() %>%
      mutate(date = format(date, "%Y-%m-%d")) %>%
      select(AQSID, date, Smoke_Intensity, Averaging_period, Value, SiteName) %>%
      datatable(options = list(pageLength = 15, lengthMenu = c(15, 30, 50), scrollX = TRUE))
  })
  
  # HMS Plot
  filteredHMSData <- reactiveVal(NULL)
  
  # Add this observe block
  observe({
    req(input$dateRange, combinedData())
    filtered_data <- combinedData() %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    filteredHMSData(filtered_data)
  })
  
  # Modified output$hmsPlot
  # Update the plot function
  output$hmsPlot <- renderPlot({
    createHMSPlot()
  }, res = 96)
  
  
  # Add this new output for downloading the HMS plot
  output$downloadHMSPlot <- downloadHandler(
    filename = function() {
      # Get the start and end dates from the dateRange input
      start_date <- format(input$dateRange[1], "%Y-%m-%d")
      end_date <- format(input$dateRange[2], "%Y-%m-%d")
      
      # Create the filename using the date range
      paste0("hms_plot_", start_date, "_", end_date, ".png")
    },
    content = function(file) {
      # Capture the plot
      plot <- createHMSPlot()
      
      # Save the plot as a PNG file
      ggsave(file, plot = plot, device = "png", width = 12, height = 8, dpi = 300)
    }
  )
  
  # Create a function to generate the HMS plot
  createHMSPlot <- reactive({
    req(filteredHMSData(), input$selectedSiteNames)
    
    tryCatch({
      # Prepare base data
      filtered_data <- filteredHMSData() %>%
        filter(!is.na(SiteName), SiteName %in% input$selectedSiteNames) %>%
        mutate(
          Averaging_period = factor(Averaging_period, levels = c(24), labels = c("24-hour")),
          Smoke_Intensity = factor(Smoke_Intensity, levels = c("Light", "Medium", "Heavy")),
          Site_ID = sub("^0", "", AQSID),
          date = as.Date(date),
          month = lubridate::month(date)
        )
      
      # Get tiering data
      tier_data_local <- tier_data()
      
      # Basic setup for plot
      color_mapping <- c("Light" = "lightblue", "Medium" = "darkgrey", "Heavy" = "black")
      date_info <- get_date_breaks(range(filtered_data$date))
      
      # Base Plot Object
      p <- ggplot(filtered_data, aes(x = date, y = Value)) +
        geom_point(aes(color = Smoke_Intensity), size = 3, na.rm = TRUE) +
        geom_text(aes(label = round(Value, 1)), vjust = -1, size = 3, na.rm = TRUE)
      
      # Add Tiers if available
      if (!is.null(tier_data_local)) {
        # Join data
        merged_data <- filtered_data %>%
          left_join(tier_data_local, by = c("Site_ID" = "SITE_ID", "month"))
        
        # Add Tier Lines
        if (any(!is.na(merged_data$Tier.1))) {
          p <- p + geom_line(data = merged_data, aes(y = Tier.1), color = "red", linetype = "dashed")
        }
        if (any(!is.na(merged_data$Tier.2))) {
          p <- p + geom_line(data = merged_data, aes(y = Tier.2), color = "blue", linetype = "dashed")
        }
        
        # Add Tier Labels
        tier_labels <- merged_data %>%
          group_by(SiteName) %>%
          summarise(
            Tier1 = mean(Tier.1, na.rm = TRUE),
            Tier2 = mean(Tier.2, na.rm = TRUE),
            max_date = max(date)
          ) %>%
          filter(!is.na(Tier1) | !is.na(Tier2))
        
        if (nrow(tier_labels) > 0) {
          p <- p + 
            geom_text(data = tier_labels, aes(x = max_date, y = Tier1, label = paste("Tier 1:", round(Tier1,1))), color="red", hjust=1, vjust=-0.5, size=3) +
            geom_text(data = tier_labels, aes(x = max_date, y = Tier2, label = paste("Tier 2:", round(Tier2,1))), color="blue", hjust=1, vjust=1.5, size=3)
        }
        
        # Update dataset for faceting
        p$data <- merged_data
      }
      
      # Final Plot Styling
      p + scale_color_manual(values = color_mapping) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = date_info$angle, hjust = 1, vjust = 0.5, size = 8),
          legend.position = "bottom"
        ) +
        labs(x = "Date", y = "PM2.5 (μg/m³)", color = "Smoke Intensity") +
        facet_wrap(~ SiteName, scales = "free_y") +
        scale_x_date(date_breaks = date_info$breaks, date_labels = date_info$labels)
      
    }, error = function(e) {
      print(paste("Error in HMS Plot:", e$message))
      ggplot() + annotate("text", x=0.5, y=0.5, label=paste("Error:", e$message)) + theme_void()
    })
  })
  
  # Filtered Data
  filteredData <- reactive({
    req(combinedData(), input$pm25Threshold, input$filteredSmokeIntensity, input$tierFilter)
    
    # 1. Base Filtering
    combined_data <- combinedData() %>%
      filter(Value >= input$pm25Threshold, Smoke_Intensity %in% input$filteredSmokeIntensity)
    
    # 2. Return early if no tier filter needed
    if (input$tierFilter == "none") {
      return(combined_data %>% select(AQSID, date, Smoke_Intensity, Averaging_period, Value, SiteName))
    }
    
    # 3. Get and Check Tier Data
    tier_data_local <- tier_data()
    
    if (is.null(tier_data_local) || !all(c("Tier.1", "Tier.2") %in% names(tier_data_local))) {
      showNotification("Tiering data unavailable. Returning unfiltered data.", type = "warning")
      return(combined_data %>% select(AQSID, date, Smoke_Intensity, Averaging_period, Value, SiteName))
    }
    
    # 4. Join and Filter
    filtered_data <- combined_data %>%
      mutate(Site_ID = sub("^0", "", AQSID), month = lubridate::month(date)) %>%
      left_join(tier_data_local, by = c("Site_ID" = "SITE_ID", "month"))
    
    if (input$tierFilter == "tier2") {
      filtered_data <- filtered_data %>% filter(!is.na(Tier.2) & Value >= Tier.2)
    } else if (input$tierFilter == "tier1") {
      filtered_data <- filtered_data %>% filter(!is.na(Tier.1) & Value >= Tier.1)
    }
    
    # Ensure CRS consistency for the join
    filtered_data_sf <- st_as_sf(filtered_data, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
    tier_data_local_sf <- st_as_sf(tier_data_local, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)
    
    # Perform spatial filter or join if needed, but here they join by SITE_ID and month
    # To suppress datum warnings, ensure everything is definitely 4326
    return(filtered_data %>% select(AQSID, date, Smoke_Intensity, Averaging_period, Value, SiteName, Tier.1, Tier.2))
  })
  
  output$filteredDataTable <- renderDT({
    req(filteredData())
    
    data_to_display <- filteredData() %>%
      mutate(date = format(date, "%Y-%m-%d"))
    
    # Check which columns are available
    available_columns <- names(data_to_display)
    numeric_columns <- intersect(c("Value", "Tier.1", "Tier.2"), available_columns)
    
    print("Columns in filtered data table:")
    print(available_columns)
    print("Numeric columns to format:")
    print(numeric_columns)
    
    # Create the datatable
    dt <- datatable(data_to_display, 
                    options = list(pageLength = 15, 
                                   lengthMenu = c(15, 30, 50), 
                                   scrollX = TRUE))
    
    # Apply formatting only if numeric columns exist
    if (length(numeric_columns) > 0) {
      dt <- formatRound(dt, columns = numeric_columns, digits = 1)
    }
    
    return(dt)
  })
  
  # Download handler for filtered data
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      start_date <- format(input$dateRange[1], "%Y-%m-%d")
      end_date <- format(input$dateRange[2], "%Y-%m-%d")
      paste0("filtered_data_", start_date, "_", end_date, ".csv")
    },
    content = function(file) {
      write.csv(filteredData() %>% 
                  mutate(date = format(date, "%Y-%m-%d")), 
                file, row.names = FALSE)
    }
  )
  
  # Process HMS Data
  observeEvent(input$processHMSData, {
    req(filteredPM25Data(), input$dateRange)
    
    withProgress(message = 'Processing HMS Smoke data', value = 0, {
      state_pm25_data <- filteredPM25Data()
      
      # Debug print
      print("PM2.5 Data before processing:")
      print(state_pm25_data %>% filter(SiteName == "GPORT YC"))
      
      if (!all(c("Latitude", "Longitude") %in% names(state_pm25_data))) {
        showNotification("Latitude and Longitude columns not found in PM2.5 data.", type = "error")
        return(NULL)
      }
      
      if (input$stateCode == "ALL") {
        state_names <- unique(state_code_to_name[unique(state_pm25_data$State_Code)])
      } else {
        state_names <- state_code_to_name[input$stateCode]
      }
      
      if (any(is.na(state_names))) {
        showNotification(paste("Invalid state code(s) found:", 
                               paste(input$stateCode[is.na(state_names)], collapse = ", ")), 
                         type = "error")
        return(NULL)
      }
      
      us_states <- maps::map("state", fill = TRUE, plot = FALSE)
      us_states_sf <- sf::st_as_sf(us_states)
      state_sf <- us_states_sf[tolower(us_states_sf$ID) %in% state_names, ]
      
      if (nrow(state_sf) == 0) {
        showNotification(paste("State(s) not found:", paste(state_names, collapse = ", ")), type = "error")
        return(NULL)
      }
      
      state_sf <- clean_geometry(state_sf)
      state_sf_transformed(sf::st_transform(state_sf, 4326))
      
      sites_sf <- st_as_sf(state_pm25_data, coords = c("Longitude", "Latitude"), crs = 4326)
      
      dates <- seq.Date(input$dateRange[1], input$dateRange[2], by = "day")
      layers <- paste0("Smoke (", input$smokeIntensity, ")")
      
      all_data <- lapply(dates, function(date) {
        incProgress(1/length(dates), detail = paste("Processing", date))
        lapply(layers, function(layer) read_kml(date, layer, state_sf_transformed(), sites_sf))
      })
      
      all_data <- do.call(rbind, Filter(Negate(is.null), unlist(all_data, recursive = FALSE)))
      
      if (is.null(all_data) || nrow(all_data) == 0) {
        showNotification(paste("No HMS Smoke data available for the selected area, dates, and smoke intensities."), type = "warning")
        return(NULL)
      }
      
      pm25_join <- state_pm25_data %>%
        select(AQSID, Valid_date, Averaging_period, Value, SiteName) %>%
        distinct()
      
      combined_data <- all_data %>%
        sf::st_drop_geometry() %>%
        select(AQSID, date, Smoke_Intensity) %>%
        distinct() %>%
        left_join(pm25_join, by = c("AQSID", "date" = "Valid_date")) %>%
        filter(!is.na(Averaging_period)) %>%
        mutate(Smoke_Intensity = gsub("Smoke \\((.*)\\)", "\\1", Smoke_Intensity))
      
      # Keep all intensities, arranged by AQSID, date, and Smoke_Intensity
      combined_data <- combined_data %>%
        arrange(AQSID, date, factor(Smoke_Intensity, levels = c("Light", "Medium", "Heavy")))
      
      print("Combined data after processing:")
      print(combined_data %>% filter(SiteName == "GPORT YC"))
      
      combinedData(combined_data)
    })
  })
  
  # Daily AQ, Met, HMS Smoke Analysis Server Logic
  # Modify the dailyData reactive
  dailyData <- eventReactive(input$generate, {
    withProgress(message = 'Generating AQ & Met data...', value = 0, {
      tryCatch({
        asos_data <- ASOS_Stations()
        if (is.null(asos_data)) {
          return(list(error = "Failed to fetch ASOS data for the selected state."))
        }
        
        list(
          air_quality = generate_combined_plot(input$date, input$state, asos_data, input$aqStateCode)
        )
      }, error = function(e) {
        list(error = paste("Error generating data:", conditionMessage(e)))
      })
    })
  })
  
  # Separate HMS plots to make the fire toggle reactive without re-running ASOS/AQ logic
  dailyHmsPlots <- eventReactive(list(input$generate, input$aqs_fires), {
    req(input$date, input$state)
    withProgress(message = 'Generating HMS Smoke Plots...', value = 0, {
       generate_hms_smoke_plots(input$date, input$state, show_fires = input$aqs_fires)
    })
  })
  
  
  # Render AQ Plot
  output$aqPlot <- renderPlot({
    req(dailyData())
    if (!is.null(dailyData()$error)) {
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, dailyData()$error, cex = 1.5)
    } else if (is.null(dailyData()$air_quality) || !is.null(dailyData()$air_quality$error)) {
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, dailyData()$air_quality$error %||% "No air quality data available", cex = 1.5)
    } else {
      grid.draw(dailyData()$air_quality$air_quality_plot)
    }
  }, height = 1600, width = 1600)  # Adjust these values as needed
  
  # Render Weather Map
  output$weatherMap <- renderPlot({
    req(dailyData())
    if (!is.null(dailyData()$error)) {
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, dailyData()$error, cex = 1.5)
    } else if (is.null(dailyData()$air_quality) || !is.null(dailyData()$air_quality$error)) {
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, dailyData()$air_quality$error %||% "No weather data available", cex = 1.5)
    } else {
      selected_map <- switch(input$mapLevel,
                             "Surface" = dailyData()$air_quality$weather_map_plot,
                             "925mb" = dailyData()$air_quality$upper_air_925_plot,
                             "850mb" = dailyData()$air_quality$upper_air_850_plot,
                             "700mb" = dailyData()$air_quality$upper_air_700_plot,
                             "500mb" = dailyData()$air_quality$upper_air_500_plot,
                             "300mb" = dailyData()$air_quality$upper_air_300_plot)
      
      if (is.null(selected_map)) {
        plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
        text(0.5, 0.5, paste("No map available for", input$mapLevel), cex = 1.5)
      } else {
        map_date <- format(as.Date(input$date) + 1, "%Y-%m-%d")
        grid.arrange(selected_map, 
                     top = textGrob(paste(input$mapLevel, "Analysis -", map_date, "00Z"), 
                                    gp = gpar(fontsize = 20, font = 2)))
      }
    }
  }, height = 1200, width = 1200)  # Adjust these values as needed
  
  # Render HMS Smoke Plots
  output$hmsSmokePlotState <- renderPlot({
    req(dailyHmsPlots())
    dailyHmsPlots()$state
  }, height = 1000, width = 1000)
  
  output$hmsSmokePlotNational <- renderPlot({
    req(dailyHmsPlots())
    dailyHmsPlots()$national
  }, height = 1000, width = 1000)
  
  # Back Trajectory Analysis Server Logic
  trajectoryAirNowData <- reactiveVal(NULL)
  
  # Function to download AirNow data for a specific date
  downloadTrajectoryAirNowData <- memoise(function(date) {
    year <- format(date, "%Y")
    yyyymmdd <- format(date, "%Y%m%d")
    url <- paste0("https://files.airnowtech.org/airnow/", year, "/", yyyymmdd, "/daily_data_v2.dat")
    
    tryCatch({
      response <- GET(url)
      if (status_code(response) == 200) {
        data <- read_delim(I(content(response, "text")), 
                           delim = "|", 
                           col_names = c("Valid_date", "AQSID", "SiteName", "Parameter_name", "Reporting_units", "Value", "Averaging_period", "Data_Source", "AQI_Value", "AQI_Category", "Latitude", "Longitude", "Full_AQSID"),
                           col_types = cols(
                             Valid_date = col_character(),
                             AQSID = col_character(),
                             SiteName = col_character(),
                             Parameter_name = col_character(),
                             Reporting_units = col_character(),
                             Value = col_double(),
                             Averaging_period = col_character(),
                             Data_Source = col_character(),
                             AQI_Value = col_integer(),
                             AQI_Category = col_character(),
                             Latitude = col_double(),
                             Longitude = col_double(),
                             Full_AQSID = col_character()
                           ))
        
        data <- data %>%
          mutate(
            Valid_date = mdy(Valid_date),
            State_Code = substr(AQSID, 1, 2),
            County_Code = substr(AQSID, 3, 5),
            Site_ID = substr(AQSID, 6, 9)
          )
        
        return(data)
      } else {
        warning(paste("Failed to download data for", yyyymmdd, "- Status code:", status_code(response)))
        return(NULL)
      }
    }, error = function(e) {
      warning(paste("Error downloading data for", yyyymmdd, ":", conditionMessage(e)))
      return(NULL)
    })
  })
  
  # Update trajectoryAirNowData when date changes
  observe({
    req(input$trajectoryDate)
    data <- downloadTrajectoryAirNowData(input$trajectoryDate)
    trajectoryAirNowData(data)
  })
  
  # Update State choices for trajectory
  observe({
    req(trajectoryAirNowData())
    data <- trajectoryAirNowData()
    
    if (!"State_Code" %in% names(data)) {
      showNotification("State_Code column not found in the data.", type = "error")
      return()
    }
    
    state_codes <- unique(data$State_Code[!is.na(data$State_Code) & data$State_Code != ""])
    
    if (length(state_codes) == 0) {
      showNotification("No valid State Codes found in the data.", type = "warning")
      updatePickerInput(session, "trajectoryStateCode", choices = character(0))
      return()
    }
    
    state_names <- state_code_to_name[state_codes]
    valid_indices <- !is.na(state_names)
    
    if (sum(valid_indices) == 0) {
      showNotification("No matching state names found for the State Codes.", type = "warning")
      updatePickerInput(session, "trajectoryStateCode", choices = character(0))
      return()
    }
    
    state_choices <- setNames(state_codes[valid_indices], tools::toTitleCase(state_names[valid_indices]))
    
    # Update choices without changing the selection
    updatePickerInput(session, "trajectoryStateCode", 
                      choices = state_choices, 
                      selected = if (is.null(values$lastSelectedState)) state_choices[1] else values$lastSelectedState)
  })
  
  # Update SiteName choices for trajectory
  observe({
    req(trajectoryAirNowData(), input$trajectoryStateCode)
    data <- trajectoryAirNowData()
    
    sitenames <- data %>%
      filter(State_Code == input$trajectoryStateCode) %>%
      distinct(SiteName) %>%
      filter(!is.na(SiteName) & SiteName != "") %>%
      pull(SiteName)
    
    if (length(sitenames) == 0) {
      showNotification("No valid SiteNames found for the selected State.", type = "warning")
    }
    
    # Update choices without changing the selection if possible
    updatePickerInput(session, "trajectorySiteName", 
                      choices = sitenames, 
                      selected = if (is.null(values$lastSelectedSiteName) || !(values$lastSelectedSiteName %in% sitenames)) sitenames[1] else values$lastSelectedSiteName)
  })
  
  # Add these new observe blocks right here, after the existing ones
  observeEvent(input$trajectoryStateCode, {
    values$lastSelectedState <- input$trajectoryStateCode
  })
  
  observeEvent(input$trajectorySiteName, {
    values$lastSelectedSiteName <- input$trajectorySiteName
  })
  
  # Get selected site information
  selectedSite <- reactive({
    req(trajectoryAirNowData(), input$trajectoryStateCode, input$trajectorySiteName)
    data <- trajectoryAirNowData()
    
    site_data <- data %>%
      filter(State_Code == input$trajectoryStateCode,
             SiteName == input$trajectorySiteName) %>%
      select(Latitude, Longitude) %>%
      distinct()
    
    if (nrow(site_data) > 0) {
      return(site_data)
    }
    return(NULL)
  })
  
  # Generate trajectory data
  trajectoryData <- reactiveVal(NULL)
  
  observeEvent(input$generateTrajectory, {
    req(selectedSite())
    
    # Validate inputs
    validation_message <- validateTrajectoryInputs(input)
    if (!is.null(validation_message)) {
      showNotification(validation_message, type = "error", duration = NULL)
      return()
    }
    
    site <- selectedSite()
    
    withProgress(message = 'Generating back trajectory and smoke overlay...', value = 0, {
      # Run the trajectory model for each selected hour
      trajectory_results <- lapply(as.numeric(input$daily_hours), function(hour) {
        run_trajectory_model(site$Latitude, site$Longitude, 
                             c(input$height1, input$height2, input$height3),
                             input$duration, input$trajectoryDate, 
                             hour,
                             input$direction, input$met_type)
      })
      
      # Check if any of the runs resulted in an error
      if (any(sapply(trajectory_results, function(x) x$status == "error"))) {
        err_msgs <- sapply(trajectory_results, function(x) if(x$status == "error") x$message else NULL)
        err_msgs <- Filter(Negate(is.null), err_msgs)
        showNotification(
          paste("Error generating trajectory:", paste(unique(err_msgs), collapse = "; ")),
          type = "error",
          duration = NULL
        )
        trajectoryData(NULL)
      } else {
        smoke_data <- parse_hms_smoke_data(input$trajectoryDate, "National")
        
        # Combine all trajectory results
        all_trajectories <- map_dfr(trajectory_results, function(result) {
          map_dfr(result$data, get_output_tbl, .id = "height_index")
        }, .id = "hour_index")
        
        all_trajectories <- all_trajectories %>%
          mutate(
            starting_height = c(input$height1, input$height2, input$height3)[as.numeric(height_index)],
            starting_hour = as.numeric(input$daily_hours)[as.numeric(hour_index)]
          )
        
        trajectoryData(list(
          trajectory = all_trajectories, 
          smoke = smoke_data, 
          timestamp = Sys.time(),
          date = input$trajectoryDate,
          sitename = input$trajectorySiteName
        ))
        trajectoryGenerated(TRUE)
      }
    })
  })
  
  trajectoryHMSData <- eventReactive(input$generateTrajectory, {
    req(input$trajectoryDate)
    parse_hms_smoke_data(input$trajectoryDate, "National")
  })
  
  
  # Render trajectory plot with national HMS Smoke overlay
  output$trajectoryPlot <- renderPlot({
    if (!trajectoryGenerated()) {
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "Welcome to the Back Trajectory Analysis!\nPlease set your parameters and click 'Generate Trajectory & Smoke' to begin.", cex = 1.2, col = "blue", adj = 0.5)
    } else if (is.null(trajectoryData())) {
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "Error generating trajectory. Please try different parameters.", cex = 1.2, col = "red")
    } else {
      data <- trajectoryData()
      plot_date <- data$date
      all_trajectories <- data$trajectory
      
      bbox <- all_trajectories %>%
        summarize(
          lon_min = min(lon) - 5,
          lon_max = max(lon) + 5,
          lat_min = min(lat) - 5,
          lat_max = max(lat) + 5
        )
      
      states_map <- map_data("state")
      
      p <- ggplot() +
        geom_polygon(data = states_map, aes(x = long, y = lat, group = group), 
                     fill = "white", color = "black", size = 0.6)
      
      if (!is.null(data$smoke$national)) {
        smoke_data_cropped <- st_crop(data$smoke$national, 
                                      xmin = bbox$lon_min, xmax = bbox$lon_max, 
                                      ymin = bbox$lat_min, ymax = bbox$lat_max)
        
        p <- p + geom_sf(data = smoke_data_cropped, aes(fill = Density), alpha = 0.5) +
          scale_fill_manual(values = c("Light" = "lightblue", "Medium" = "grey", "Heavy" = "darkgrey"),
                            name = "Smoke Density")
      }
      
      p <- p +
        geom_path(data = all_trajectories, 
                  aes(x = lon, y = lat, 
                      color = factor(starting_height),
                      group = interaction(starting_height, starting_hour)),
                  size = 1) +
        geom_point(data = all_trajectories, 
                   aes(x = lon, y = lat, 
                       color = factor(starting_height),
                       shape = factor(starting_hour)),
                   size = 3) +
        coord_sf(xlim = c(bbox$lon_min, bbox$lon_max),
                 ylim = c(bbox$lat_min, bbox$lat_max)) +
        theme_minimal() +
        labs(title = paste("Trajectory Plot with National HMS Smoke Overlay\n",
                           "Date:", format(plot_date, "%Y-%m-%d"),
                           "- SiteName:", data$sitename),
             x = "Longitude", 
             y = "Latitude", 
             color = "Starting Height (m)",
             shape = "Starting Hour") +
        scale_color_brewer(palette = "Set1") +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.position = "bottom",
          plot.title = element_text(size = 16, hjust = 0.5, lineheight = 1.2)
        )
      
      state_centers <- states_map %>%
        group_by(region) %>%
        summarize(long = mean(long), lat = mean(lat))
      
      p + geom_text_repel(data = state_centers %>% 
                            filter(long >= bbox$lon_min, long <= bbox$lon_max,
                                   lat >= bbox$lat_min, lat <= bbox$lat_max),
                          aes(x = long, y = lat, label = region),
                          size = 4, alpha = 0.7)
    }
  }, height = 1000, width = 1000)
  
  output$crossSectionPlot <- renderPlot({
    if (!trajectoryGenerated()) {
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "Cross-section plot will appear here after generating the trajectory.", cex = 1.2, col = "blue")
    } else if (is.null(trajectoryData())) {
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "Error generating trajectory. Please try different parameters.", cex = 1.2, col = "red")
    } else {
      data <- trajectoryData()
      plot_date <- data$date
      all_trajectories <- data$trajectory
      
      all_trajectories <- all_trajectories %>%
        group_by(starting_height, starting_hour) %>%
        mutate(
          time_hours = as.numeric(difftime(traj_dt, first(traj_dt), units = "hours"))
        ) %>%
        ungroup()
      
      ggplot(all_trajectories, aes(x = time_hours, y = height, 
                                   color = factor(starting_height),
                                   linetype = factor(starting_hour))) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        theme_minimal() +
        labs(title = paste("Trajectory Cross-section -", format(plot_date, "%Y-%m-%d")), 
             x = "Time (hours)", 
             y = "Height (m)", 
             color = "Starting Height (m)",
             linetype = "Starting Hour") +
        scale_color_brewer(palette = "Set1") +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.position = "bottom",
          plot.title = element_text(size = 14, hjust = 0.5),
          aspect.ratio = 0.4
        ) +
        scale_x_reverse() +
        coord_cartesian(expand = FALSE)
    }
  }, height = 400, width = 800)
  
  # In the UI, adjust the height of the crossSectionPlot:
  
  fluidRow(
    plotOutput("crossSectionPlot", height = "250px")
  )
  
  ## START of new code snippet (Export and Summary Logic) ##
  # Add export all data functionality
  output$exportAllData <- downloadHandler(
    filename = function() {
      paste0("air_quality_analysis_export_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      
      if (!is.null(pm25Data())) {
        openxlsx::addWorksheet(wb, "PM25_Data")
        openxlsx::writeData(wb, "PM25_Data", pm25Data())
      }
      if (!is.null(combinedData())) {
        openxlsx::addWorksheet(wb, "Combined_HMS_Data")
        openxlsx::writeData(wb, "Combined_HMS_Data", combinedData())
      }
      if (!is.null(filteredData())) {
        openxlsx::addWorksheet(wb, "Filtered_Data")
        openxlsx::writeData(wb, "Filtered_Data", filteredData())
      }
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  # Add after you  # Unified data source for the Data Summary Dashboard
  dashboardData <- reactiveVal(NULL)
  
  # Sync from Tab 3 (HMS Smoke Analysis)
  observeEvent(pm25Data(), { dashboardData(pm25Data()) })
  
  # Independent Fetch for Tab 6 (Now using official daily_data_v2)
  observeEvent(input$summarize_btn, {
    req(input$summary_dates, input$summary_state)
    withProgress(message = "Fetching Official Daily Data...", value = 0, {
      d1 <- input$summary_dates[1]
      d2 <- input$summary_dates[2]
      states <- input$summary_state
      
      # Use fetch_airnow_daily for the dashboard summary (official records)
      processed <- fetch_airnow_daily(d1, d2, states = states)
      
      if (is.null(processed) || nrow(processed) == 0) {
        showNotification("No official daily data found for the selected range/states.", type = "warning")
        return()
      }
      
      # Ensure column naming parity for the dashboard logic (mapping both Hourly and Daily fields)
      processed <- processed %>%
        mutate(
          Valid_date = if("date" %in% names(.)) date else as.Date(ValidDate),
          Value = if("pm25" %in% names(.)) pm25 else PM25,
          AQI_Category = if("AQI_Cat" %in% names(.)) AQI_Cat else aqi_cat(Value),
          State_Code = if("StateName" %in% names(.)) StateName else StateName,
          SiteName = if("SiteName" %in% names(.)) SiteName else if("City" %in% names(.)) City else "Unknown",
          AQSID = if("Monitoring_Site_ID" %in% names(.)) Monitoring_Site_ID else AQSID
        )
      dashboardData(processed)
    })
  })

  # Reactive expression for summarized data, which will react to map clicks
  summaryData <- reactive({
    data <- dashboardData()
    req(data)
    
    # Filter by selected state from the map
    selected_state <- input$stateMap_shape_click$id
    if (!is.null(selected_state) && selected_state != "All" && selected_state %in% unique(data$State_Code)) {
      data <- data[data$State_Code == selected_state, ]
    }
    return(data)
  })
  
  output$totalRecords <- renderValueBox({
    d <- summaryData()
    bslib::value_box(
      title = "Total PM2.5 Records",
      value = if(is.null(d)) 0 else nrow(d),
      showcase = icon("database"),
      theme = "primary"
    )
  })
  
  output$dateRangeBox <- renderValueBox({
    d <- summaryData()
    if (is.null(d) || nrow(d) == 0) {
      date_range_text <- "No data"
    } else {
      date_range <- range(d$Valid_date, na.rm = TRUE)
      date_range_text <- paste(format(date_range[1], "%Y-%m-%d"), "-",
                                format(date_range[2], "%Y-%m-%d"))
    }
    bslib::value_box(
      title = "Date Range",
      value = date_range_text,
      showcase = icon("calendar"),
      theme = "success"
    )
  })
  
  output$statesCount <- renderValueBox({
    d <- summaryData()
    bslib::value_box(
      title = "States with Data",
      value = if(is.null(d)) 0 else length(unique(d$State_Code)),
      showcase = icon("map"),
      theme = "info"
    )
  })
  
  output$missingValuesBox <- renderValueBox({
    data <- summaryData()
    if (is.null(data) || nrow(data) == 0) {
      missing_pct <- 0
    } else {
      missing_pct <- round(sum(is.na(data$Value)) / nrow(data) * 100, 2)
    }
    bslib::value_box(
      title = "Missing PM2.5 Values",
      value = paste0(missing_pct, "%"),
      showcase = icon("exclamation-triangle"),
      theme = "warning"
    )
  })
  
  output$dailyCountPlot <- renderPlot({
    req(summaryData())
    summaryData() %>%
      count(Valid_date) %>%
      ggplot(aes(x = Valid_date, y = n)) +
      geom_bar(stat = "identity", fill = "dodgerblue") +
      labs(title = "Daily Record Count", x = "Date", y = "Number of Records") +
      theme_minimal()
  })
  
  output$dataQualityTable <- renderDT({
    req(summaryData())
    data <- summaryData()
    
    # Ensure Value exists (mapped from PM25)
    val <- if("Value" %in% names(data)) data$Value else data$PM25
    
    quality_report <- data.frame(
      Metric = c("Total Records", "Missing Timestamps", "Missing AQSID", "Missing PM2.5 Values", "Min PM2.5", "Max PM2.5", "Mean PM2.5"),
      Value = c(
        nrow(data),
        sum(is.na(data$Valid_date)),
        sum(is.na(data$AQSID)),
        sum(is.na(val)),
        if(all(is.na(val))) NA else min(val, na.rm = TRUE),
        if(all(is.na(val))) NA else max(val, na.rm = TRUE),
        if(all(is.na(val))) NA else mean(val, na.rm = TRUE)
      )
    )
    
    datatable(quality_report, options = list(dom = 't'), rownames = FALSE) %>%
      formatRound(columns = 'Value', digits = 2)
  })
  
  output$stateMap <- renderLeaflet({
    d <- dashboardData()
    req(d)
    state_counts <- as.data.frame(table(d$State_Code))
    names(state_counts) <- c("State_Code", "Record_Count")
    
    # Get state geometries
    states_map <- states(cb = TRUE)
    states_map <- states_map %>% filter(NAME %in% state.name)
    
    # Use names for join to matches dashboardData structure
    states_map <- merge(states_map, state_counts, by.x = "NAME", by.y = "State_Code")
    
    pal <- colorNumeric(palette = "viridis", domain = states_map$Record_Count)
    
    leaflet(states_map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Record_Count),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0(NAME, ": ", Record_Count, " records"),
        layerId = ~NAME
      ) %>%
      addLegend(pal = pal, values = ~Record_Count, opacity = 0.7, title = "Records",
                position = "bottomright")
  })
  
  output$pm25DistributionPlot <- renderPlot({
    req(summaryData())
    ggplot(summaryData(), aes(x = Value)) +
      geom_histogram(binwidth = 5, fill = "darkorange", color = "black", alpha = 0.7) +
      # Add text labels on top of each bar
      geom_text(
        stat = "bin", 
        aes(label = after_stat(count)), 
        binwidth = 5, 
        vjust = -0.5, 
        size = 3.5
      ) +
      labs(title = "Distribution of PM2.5 Values", x = "PM2.5 (µg/m³)", y = "Frequency") +
      theme_minimal()
  })
  
  output$aqiCategoryPlot <- renderPlot({
    req(summaryData())
    
    # 1. Define the AQI categories and corresponding colors
    pm25_breaks <- c(-Inf, 9, 35.4, 55.4, Inf)
    pm25_labels <- c("Good", "Moderate", "USG", "Unhealthy")
    aqi_colors <- c("Good" = "#00e400", "Moderate" = "#ffff00", "USG" = "#ff7e00", "Unhealthy" = "#ff0000")
    
    # 2. Process the data
    aqi_data <- summaryData() %>%
      filter(!is.na(Value) & !is.na(SiteName)) %>%
      mutate(
        Category = cut(Value, breaks = pm25_breaks, labels = pm25_labels, right = FALSE)
      ) %>%
      group_by(SiteName, Category) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(
        Category = factor(Category, levels = rev(pm25_labels))
      )
    
    if (nrow(aqi_data) == 0) {
      return(
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected area.") +
          theme_void()
      )
    }
    
    # 3. Create the stacked bar plot
    ggplot(aqi_data, aes(x = SiteName, y = Count, fill = Category)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(
        aes(label = ifelse(Count > 0, Count, "")), 
        position = position_stack(vjust = 0.5), 
        size = 3.5, 
        color = "black"
      ) +
      scale_fill_manual(values = aqi_colors, name = "AQI Category") +
      labs(
        title = "AQI Category Count by Monitoring Site",
        subtitle = "Filtered by state selection from the map",
        x = "SiteName",
        y = "Number of Records"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        # Adjust site name font size slightly
        axis.text.y = element_text(size = 9.5) 
      ) +
      coord_flip()
    
    # This 'height' function makes the magic happen
  }, height = function() {
    data <- summaryData()
    # Return a default height if no data is present
    if (is.null(data) || nrow(data) == 0) {
      return(450)
    }
    # Calculate the number of unique sites that will be plotted
    num_sites <- length(unique(data$SiteName))
    # Calculate a dynamic height: 35px per site + 100px for margins, titles, etc.
    # Set a minimum height of 450px and a maximum of 15000px to prevent rendering crashes.
    min(15000, max(450, num_sites * 35 + 100))
  })


  get_msa_bbox <- function(msa_key, custom_addr = NULL, custom_radius_km = 150) {
    cfg <- msa_definitions[[msa_key]]
    if (!is.null(cfg$bbox_coords)) return(cfg$bbox_coords)
    if (!is.null(custom_addr) && nchar(trimws(custom_addr)) > 0) {
      loc <- suppressWarnings(geocode(tibble(a = custom_addr), address = "a", method = "osm", quiet = TRUE))
      if (!is.na(loc$lat)) {
        pt  <- st_as_sf(loc, coords = c("long","lat"), crs = 4326)
        buf <- pt %>% st_transform(5070) %>% st_buffer(custom_radius_km * 1000) %>% st_transform(4326)
        bb  <- st_bbox(buf)
        return(c(xmin = bb["xmin"], ymin = bb["ymin"], xmax = bb["xmax"], ymax = bb["ymax"]))
      }
    }
    NULL
  }

  # ==========================================================
  # TAB 1 — AirNow MSA Analysis
  # ==========================================================
  msa_rv <- reactiveValues(ts = NULL, diurnal = NULL, raw = NULL, summary = NULL)

  observeEvent(input$run_msa, {
    req(input$msa_dates, input$msa_select)
    cfg  <- msa_definitions[[input$msa_select]]
    tz   <- get_olson_tz(if (input$msa_select == "-- Custom Location --") input$msa_custom_gmt else cfg$gmt_offset)
    bbox <- get_msa_bbox(input$msa_select, input$msa_custom_addr,
                         if (!is.null(input$msa_custom_radius)) input$msa_custom_radius else 150)
    if (is.null(bbox)) { showNotification("Could not resolve location.", type = "error"); return() }

    withProgress(message = "Fetching AirNow PM2.5...", value = 0, {
      msa_start <- min(input$msa_dates)
      msa_end   <- max(input$msa_dates)
      states    <- if (!is.null(cfg$state_code)) cfg$state_code else NULL
      pm_raw    <- fetch_airnow_hourly(msa_start, msa_end + 1, states = states)
      if (nrow(pm_raw) == 0) { showNotification("No AirNow data found.", type = "error"); return() }

      setProgress(0.2, detail = "Fetching Official Daily Averages...")
      pm_daily  <- fetch_airnow_daily(msa_start, msa_end, bbox = bbox)

      pm_raw <- pm_raw %>%
        filter(Longitude >= bbox["xmin"], Longitude <= bbox["xmax"],
               Latitude  >= bbox["ymin"], Latitude  <= bbox["ymax"]) %>%
        mutate(
          AQSID_Full = if_else(nchar(AQSID) == 9,
                         paste(substr(AQSID,1,2), substr(AQSID,3,5), substr(AQSID,6,9), sep="-"),
                         AQSID),
          DateTimeGMT   = mdy_hm(paste(ValidDate, sprintf("%02d:00", Hour_UTC)), tz = "GMT"),
          DateTimeLocal = with_tz(DateTimeGMT, tz),
          AQI_Cat       = aqi_cat(PM25)
        ) %>%
        # Strict local date filtering to avoid UTC-shift overlap
        # Using POSIXct boundaries with explicit tz is safer than as.Date()
        filter(DateTimeLocal >= as.POSIXct(paste(msa_start, "00:00:00"), tz = tz),
               DateTimeLocal <= as.POSIXct(paste(msa_end,   "23:59:59"), tz = tz))
      msa_rv$raw <- pm_raw

      # HMS Smoke
      smoke_sf <- NULL
      if (input$msa_smoke_overlay == "y") {
        setProgress(0.4, detail = "Fetching HMS Smoke...")
        smoke_days <- seq(msa_start, msa_end + 1, by = "day")
        smoke_list <- lapply(smoke_days, function(d) {
          s <- fetch_hms_smoke_day(d)
          if (!is.null(s) && nrow(s) > 0) {
            aoi <- st_as_sfc(st_bbox(c(
              xmin = as.numeric(bbox["xmin"]), ymin = as.numeric(bbox["ymin"]),
              xmax = as.numeric(bbox["xmax"]), ymax = as.numeric(bbox["ymax"])
            ), crs = 4326))
            s_f <- st_filter(s, aoi, .predicate = st_intersects)
            if (nrow(s_f) > 0) { s_f$smoke_date <- d; return(s_f) }
          }
          NULL
        })
        smoke_list <- Filter(Negate(is.null), smoke_list)
        if (length(smoke_list) > 0) {
          smoke_sf <- do.call(rbind, smoke_list)
          parse_hms_t <- function(x) {
            if (is.na(x) || !grepl("^[0-9]{7} [0-9]{4}$", x)) return(as.POSIXct(NA, tz = "GMT"))
            yr <- substr(x,1,4); jd <- as.integer(substr(x,5,7)); hm <- substr(x,9,12)
            dt <- as.Date(paste0(yr,"-01-01")) + days(jd - 1)
            ymd_hm(paste(format(dt,"%Y-%m-%d"), substr(hm,1,2), substr(hm,3,4)), tz = "GMT")
          }
          # FIX v3: replaced map_vec() with base do.call(c, lapply()) for purrr compatibility
          smoke_sf <- smoke_sf %>%
            mutate(
              StartUTC   = do.call(c, lapply(as.character(Start), parse_hms_t)),
              EndUTC     = do.call(c, lapply(as.character(End),   parse_hms_t)),
              StartLocal = with_tz(StartUTC, tz),
              EndLocal   = with_tz(EndUTC,   tz),
              Density    = str_to_title(Density)
            )
        }
      }

      setProgress(0.8, detail = "Building plots...")
      site_cols <- scales::hue_pal()(length(unique(pm_raw$SiteName)))
      names(site_cols) <- unique(pm_raw$SiteName)

      # Time series
      p_ts <- plot_ly()
      smoke_shapes <- list()
      if (!is.null(smoke_sf) && nrow(smoke_sf) > 0) {
        smoke_colors <- c(Light  = "rgba(200,200,200,0.35)",
                          Medium = "rgba(128,128,128,0.45)",
                          Heavy  = "rgba(64,64,64,0.55)")
        for (i in seq_len(nrow(smoke_sf))) {
          row <- smoke_sf[i, ]
          if (!is.na(row$StartLocal) && !is.na(row$EndLocal)) {
            dens <- as.character(row$Density)
            smoke_shapes[[length(smoke_shapes) + 1]] <- list(
              type = "rect", fillcolor = unname(smoke_colors[dens]),
              line = list(width = 0),
              x0 = row$StartLocal, x1 = row$EndLocal, xref = "x",
              y0 = 0, y1 = 1, yref = "paper", layer = "below"
            )
          }
        }
      }
      for (site in unique(pm_raw$SiteName)) {
        df_s <- pm_raw %>% filter(SiteName == site)
        p_ts <- p_ts %>%
          add_trace(data = df_s, x = ~DateTimeLocal, y = ~PM25,
                    type = "scatter", mode = "lines+markers", name = site,
                    hovertemplate = paste0("<b>",site,"</b><br>%{x}<br>PM2.5: %{y:.1f} \u00b5g/m\u00b3<extra></extra>"))
      }

      # FIX v3: convert threshold segment endpoints to POSIXct to match x-axis type
      seg_start <- as.POSIXct(paste(msa_start, "00:00:00"), tz = tz)
      seg_end   <- as.POSIXct(paste(msa_end,   "23:59:59"), tz = tz)

      p_ts <- p_ts %>%
        add_segments(x = seg_start, xend = seg_end,
                     y = 9.0, yend = 9.0,
                     line = list(dash = "dash", color = "darkorange"),
                     name = "9 \u00b5g/m\u00b3 threshold", showlegend = TRUE) %>%
        layout(
          title    = paste("Hourly PM2.5 \u2014", if (!is.null(cfg$name)) cfg$name else input$msa_select),
          shapes   = smoke_shapes,
          xaxis    = list(title = "Local Time"),
          yaxis    = list(title = "PM2.5 \u00b5g/m\u00b3"),
          hovermode = "x unified",
          legend   = list(orientation = "h", y = -0.15)
        )
      msa_rv$ts <- p_ts

      # Diurnal
      diurnal_df <- pm_raw %>%
        filter(!is.na(SiteName)) %>%
        mutate(Hr = hour(DateTimeLocal)) %>%
        group_by(SiteName, Hr) %>%
        summarise(Avg = mean(PM25, na.rm = TRUE), .groups = "drop")

      seg_start_d <- 0; seg_end_d <- 23
      p_diurnal <- plot_ly(diurnal_df, x = ~Hr, y = ~Avg, color = ~SiteName,
                           type = "scatter", mode = "lines+markers",
                           hovertemplate = "Hour %{x}:00<br>Avg PM2.5: %{y:.1f} \u00b5g/m\u00b3<extra></extra>") %>%
        add_segments(x = seg_start_d, xend = seg_end_d, y = 9.0, yend = 9.0,
                     line = list(dash = "dash", color = "darkorange"),
                     name = "9 \u00b5g/m\u00b3", inherit = FALSE, showlegend = TRUE) %>%
        layout(title = "Average Diurnal PM2.5 Profile",
               xaxis = list(title = "Hour of Day (local)"),
               yaxis = list(title = "Avg PM2.5 \u00b5g/m\u00b3"))
      msa_rv$diurnal <- p_diurnal

      daily_means <- pm_raw %>%
        filter(!is.na(SiteName)) %>%
        mutate(DateLocal = as.Date(DateTimeLocal)) %>%
        group_by(SiteName, DateLocal) %>%
        summarise(DailyAvg = mean(PM25, na.rm = TRUE), .groups = "drop") %>%
        group_by(SiteName) %>%
        summarise(Days_Above_9 = sum(DailyAvg > 9.0, na.rm = TRUE), .groups = "drop")

      msa_rv$summary <- pm_raw %>%
        filter(!is.na(SiteName)) %>%
        group_by(SiteName) %>%
        summarise(
          N_Obs     = n(),
          Mean_PM25 = round(mean(PM25, na.rm = TRUE), 1),
          Max_Val   = max(PM25,  na.rm = TRUE),
          Max_Time  = if(n() > 0) DateTimeLocal[which.max(PM25)] else as.POSIXct(NA),
          Max_PM25  = if(all(is.na(PM25))) "NA" else paste0(round(Max_Val, 1), " (", format(Max_Time, "%m/%d %H:00"), ")"),
          Min_PM25  = if(all(is.na(PM25))) NA else round(min(PM25,  na.rm = TRUE), 1),
          .groups = "drop"
        ) %>%
        left_join(daily_means, by = "SiteName")

      # NEW: Add Official 24-hour average for the LAST FULL DAY (from daily_data_v2)
      if (!is.null(pm_daily) && nrow(pm_daily) > 0) {
        last_day_data <- pm_daily %>%
          group_by(Monitoring_Site_ID) %>%
          filter(date == max(date)) %>%
          summarise(Selected_Day = as.character(max(date)),
                    Recent_24hr_Avg = round(mean(pm25, na.rm = TRUE), 1), .groups = "drop") %>%
          rename(AQSID_Full = Monitoring_Site_ID)

        # Map AQSID_Full back to SiteName using pm_raw for the table join
        site_map <- pm_raw %>% select(SiteName, AQSID_Full) %>% distinct()
        last_day_data <- last_day_data %>% inner_join(site_map, by = "AQSID_Full") %>% select(-AQSID_Full)

        msa_rv$summary <- msa_rv$summary %>%
          left_join(last_day_data, by = "SiteName")
      } else {
        # Fallback if no daily data: Use the hourly calculation
        pm_raw_dates <- pm_raw %>% mutate(DateLocal = as.Date(DateTimeLocal))
        last_day_data <- pm_raw_dates %>%
          filter(!is.na(SiteName)) %>%
          group_by(SiteName, DateLocal) %>%
          summarise(n = n(), avg = mean(PM25, na.rm = TRUE), .groups = "drop") %>%
          group_by(SiteName) %>%
          filter(if(any(n >= 18)) n >= 18 else TRUE) %>%
          filter(DateLocal == max(DateLocal)) %>%
          summarise(Selected_Day = as.character(DateLocal),
                    Recent_24hr_Avg = round(avg, 1), .groups = "drop")

        msa_rv$summary <- msa_rv$summary %>%
          left_join(last_day_data, by = "SiteName")
      }
      
      # Final column selection for clarity
      msa_rv$summary <- msa_rv$summary %>%
        select(SiteName, N_Obs, Mean_PM25, Max_PM25, Min_PM25, 
               `Days > 9.0` = Days_Above_9, 
               `Recent 24-hr Avg` = Recent_24hr_Avg,
               `Obs Date` = Selected_Day)
    })
  })

  output$msa_ts_plot      <- renderPlotly({ req(msa_rv$ts);      msa_rv$ts })
  output$msa_diurnal_plot <- renderPlotly({ req(msa_rv$diurnal); msa_rv$diurnal })
  output$msa_summary_tbl  <- renderDT({
    req(msa_rv$summary)
    datatable(msa_rv$summary, options = list(pageLength = 15))
  })
  output$dl_msa_data    <- downloadHandler("msa_pm25_raw.csv",
    function(f) write.csv(msa_rv$raw,     f, row.names = FALSE))
  output$dl_msa_summary <- downloadHandler("msa_pm25_summary.csv",
    function(f) write.csv(msa_rv$summary, f, row.names = FALSE))

  # ==========================================================
  # TAB 2 — Hourly Cross-Section  (FIX: dynamic timezone)
  # ==========================================================
  hourly_rv <- reactiveValues(chart = NULL, map_data = NULL, fire_prox = NULL)

  observeEvent(input$run_hourly, {
    req(input$hourly_date, input$hourly_location)
    withProgress(message = "Processing...", value = 0, {
      dt     <- as.Date(input$hourly_date)
      # FIX v3: use user-supplied GMT offset, not hardcoded -6
      offset <- as.integer(input$hourly_gmt_offset)
      tz     <- get_olson_tz(offset)

      incProgress(0.2, detail = "Geocoding location...")
      loc <- suppressWarnings(geocode(tibble(a = input$hourly_location), address = "a", method = "osm", quiet = TRUE))
      if (is.na(loc$lat)) { showNotification("Geocoding failed.", type = "error"); return() }
      pt  <- st_as_sf(loc, coords = c("long","lat"), crs = 4326)
      buf <- pt %>% st_transform(5070) %>% st_buffer(input$hourly_radius * 1000) %>% st_transform(4326)

      incProgress(0.1, detail = "Fetching HMS Smoke...")
      smoke_sf_full_d1 <- fetch_hms_smoke_day(dt)
      smoke_sf_full_d2 <- fetch_hms_smoke_day(dt + 1)
      smoke_sf_full <- bind_rows(smoke_sf_full_d1, smoke_sf_full_d2)
      
      smoke_sf_local <- if (!is.null(smoke_sf_full) && nrow(smoke_sf_full) > 0) {
        res <- st_filter(smoke_sf_full, buf, .predicate = st_intersects)
        if (nrow(res) > 0) {
           res %>% 
             mutate(
               # HMS Start/End are typically strings like "2026110 0215" (YearJulDay HHMM)
               StartUTC = as.POSIXct(as.character(Start), format="%Y%j %H%M", tz="UTC"),
               EndUTC   = as.POSIXct(as.character(End),   format="%Y%j %H%M", tz="UTC"),
               StartLocal = with_tz(StartUTC, tz),
               EndLocal   = with_tz(EndUTC, tz)
             ) %>%
             # Filter for layers that overlap our local day
             filter(StartLocal < as.POSIXct(paste(dt+1, "00:00:00"), tz=tz),
                    EndLocal   > as.POSIXct(paste(dt, "00:00:00"), tz=tz)) %>%
             mutate(
               HourLocal = hour(StartLocal),
               Density   = str_to_title(Density)
             )
        } else res
      } else NULL

      incProgress(0.1, detail = "Fetching HMS Fires...")
      fires_sf <- if (input$hourly_fires_toggle) {
        f1 <- fetch_hms_fires_day(dt)
        f2 <- fetch_hms_fires_day(dt + 1)
        bind_rows(f1, f2)
      } else NULL
      fire_prox <- if (!is.null(fires_sf) && nrow(fires_sf) > 0) calc_fire_proximity(fires_sf, pt, input$hourly_radius) else NULL

      incProgress(0.2, detail = "Fetching AirNow PM2.5...")
      pm_raw <- fetch_airnow_hourly(dt, dt + 1, states = c("MS","TN","AR","AL","LA","GA","FL"))
      nearby_pm <- if (nrow(pm_raw) > 0) {
        pm_raw_local <- pm_raw %>%
          mutate(DateTimeGMT   = mdy_hm(paste(ValidDate, sprintf("%02d:00", Hour_UTC)), tz = "GMT"),
                 DateTimeLocal = with_tz(DateTimeGMT, tz)) %>%
          filter(as.Date(DateTimeLocal, tz = tz) == dt) %>%
          mutate(HourLocal = hour(DateTimeLocal))
        
        if (nrow(pm_raw_local) == 0) return(data.frame())
        
        pm_sf <- st_as_sf(pm_raw_local, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)
        st_drop_geometry(st_filter(pm_sf, buf, .predicate = st_intersects))
      } else data.frame()

      # Build hourly density summary
      if (!is.null(smoke_sf_local) && nrow(smoke_sf_local) > 0) {
        df_dens <- smoke_sf_local %>% st_drop_geometry() %>%
          filter(!is.na(HourLocal)) %>%
          group_by(HourLocal) %>%
          summarise(MaxDensity = as.character(
            max(factor(Density, levels = c("Light","Medium","Heavy"), ordered = TRUE), na.rm = TRUE)
          ), .groups = "drop")
      } else {
        df_dens <- data.frame(HourLocal = integer(), MaxDensity = character())
      }

      smoke_cols <- c(Light  = "rgba(200,200,200,0.4)",
                      Medium = "rgba(120,120,120,0.5)",
                      Heavy  = "rgba(60,60,60,0.6)")

      p_chart <- plot_ly()
      hourly_shapes <- list()

      # Apply local hour logic for plotting
      if (nrow(df_dens) > 0) {
        for (i in seq_len(nrow(df_dens))) {
          hr <- df_dens$HourLocal[i]
          d  <- df_dens$MaxDensity[i]
          hourly_shapes[[length(hourly_shapes) + 1]] <- list(
            type = "rect", fillcolor = unname(smoke_cols[d]), line = list(width = 0),
            x0 = hr, x1 = hr + 1, xref = "x",
            y0 = 0, y1 = 1, yref = "paper", layer = "below"
          )
        }
      }
      if (nrow(nearby_pm) > 0) {
        for (site in unique(nearby_pm$SiteName)) {
          df_s <- nearby_pm %>% filter(SiteName == site) %>% arrange(HourLocal)
          p_chart <- p_chart %>%
            add_trace(data = df_s,
                      x    = ~HourLocal,
                      y    = ~PM25,
                      type = "scatter", mode = "lines+markers", name = site,
                      hovertemplate = "<b>%{fullData.name}</b><br>Hour: %{x}:00<br>PM2.5: %{y:.1f}<extra></extra>")
        }
      }

      # Standard 0-23 LST scale
      local_hours <- seq(0, 23, 2)
      tick_labels <- sprintf("%02d:00", local_hours)

      tz_label <- paste0("UTC", if (offset < 0) offset else paste0("+", offset))
      p_chart <- p_chart %>%
        layout(
          title  = paste("HMS Smoke & PM2.5 for", dt, "near", input$hourly_location),
          shapes = hourly_shapes,
          xaxis  = list(title     = paste("Local Hour (", tz_label, ")", sep = ""),
                        tickvals  = local_hours,
                        ticktext  = tick_labels),
          yaxis  = list(title = "PM2.5 \u00b5g/m\u00b3"),
          hovermode = "x unified"
        )
      hourly_rv$chart     <- p_chart
      hourly_rv$map_data  <- list(pt = pt, buf = buf, smoke_sf = smoke_sf_full,
                                  fires_sf = fires_sf, nearby_pm = pm_raw)
      hourly_rv$fire_prox <- fire_prox
    })
  })

  output$hourly_chart <- renderPlotly({ req(hourly_rv$chart); hourly_rv$chart })

  output$hourly_map <- renderLeaflet({
    req(hourly_rv$map_data)
    md       <- hourly_rv$map_data
    pt_coords <- st_coordinates(md$pt)
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron",  group = "Basemap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      setView(lng = unname(pt_coords[1,1]), lat = unname(pt_coords[1,2]), zoom = 7)

    if (!is.null(md$smoke_sf) && nrow(md$smoke_sf) > 0) {
      sp <- colorFactor(c("#D3D3D3","#808080","#404040"),
                        levels = c("Light","Medium","Heavy"), na.color = "transparent")
      m <- m %>%
        addPolygons(data = md$smoke_sf, fillColor = ~sp(Density),
                    fillOpacity = 0.5, color = "#555555", weight = 0.8,
                    popup = ~paste0("<b>Smoke Density:</b> ", Density), group = "HMS Smoke") %>%
        addLegend("bottomright", pal = sp, values = c("Light","Medium","Heavy"),
                  title = "HMS Smoke", group = "HMS Smoke")
    }
    if (!is.null(md$fires_sf) && nrow(md$fires_sf) > 0) {
      fc      <- st_coordinates(md$fires_sf)
      fire_df <- st_drop_geometry(md$fires_sf)
      m <- m %>%
        addCircleMarkers(lng = fc[,1], lat = fc[,2], radius = 3,
                         color = "#e74c3c", fillOpacity = 0.7, stroke = FALSE,
                         popup = if ("FRP" %in% names(fire_df)) paste0("FRP: ", fire_df$FRP) else "HMS Fire",
                         group = "HMS Fires") %>%
        addLegend("bottomright", colors = "#e74c3c", labels = "HMS Fire", group = "HMS Fires")
    }
    m <- m %>%
      addPolygons(data = md$buf, fill = FALSE, color = "blue", weight = 2,
                  dashArray = "6", group = "Search Radius") %>%
      addCircleMarkers(lng = unname(pt_coords[1,1]), lat = unname(pt_coords[1,2]),
                       radius = 8, color = "darkgreen", fillColor = "darkgreen",
                       fillOpacity = 1, popup = input$hourly_location, group = "Location")
    m %>% addLayersControl(
      baseGroups    = c("Basemap","Satellite"),
      overlayGroups = c("HMS Smoke","HMS Fires","Search Radius","Location"),
      options       = layersControlOptions(collapsed = FALSE))
  })

  # FIX: Instant fire toggle for Tab 2
  observe({
    proxy <- leafletProxy("hourly_map")
    if (isTRUE(input$hourly_fires_toggle)) {
      proxy %>% showGroup("HMS Fires")
    } else {
      proxy %>% hideGroup("HMS Fires")
    }
  })

  output$hourly_fire_summary <- renderText({
    req(hourly_rv$fire_prox)
    fp <- hourly_rv$fire_prox
    paste0(
      "Fire Proximity Summary\n",
      "======================\n",
      "Nearest fire:   ", fp$nearest_km, " km (", fp$nearest_bearing, ")\n\n",
      "Within 10 km:   ", fp$within_10, "\n",
      "Within 25 km:   ", fp$within_25, "\n",
      "Within 50 km:   ", fp$within_50, "\n",
      "Within 100 km:  ", fp$within_100, "\n",
      "Within 250 km:  ", fp$within_250, "\n",
      "Within 500 km:  ", fp$within_500
    )
  })

  output$hourly_fire_tbl <- renderDT({
    req(hourly_rv$fire_prox)
    fp <- hourly_rv$fire_prox
    if (!is.null(fp$fires_df))
      datatable(fp$fires_df, options = list(pageLength = 8, scrollX = TRUE))
  })

  # ==========================================================
  # TAB 3 — Multi-Day AQS Smoke Events
  # ==========================================================
  aqs_rv <- reactiveValues(events = NULL, highlight = NULL, summary_p = NULL)

  observeEvent(input$run_aqs, {
    req(input$aqs_dates, input$aqs_state)
    withProgress(message = "Fetching PM2.5 data...", value = 0.1, {
      st_code <- state_name_to_code[input$aqs_state]
      
      if (input$aqs_src == "aqs") {
        req(input$aqs_email, input$aqs_key)
        myu   <- create_user(email = input$aqs_email, key = input$aqs_key)
        yseq  <- year(input$aqs_dates[1]):year(input$aqs_dates[2])
        aqs_raw <- map_dfr(yseq, ~aqs_dailyData(myu, "byState", "88101",
          paste0(.x,"0101"), paste0(.x,"1231"), state = st_code))
        filt <- aqs_raw %>%
          mutate(date = as.Date(date_local)) %>%
          filter(date >= input$aqs_dates[1], date <= input$aqs_dates[2]) %>%
          mutate(Monitoring_Site_ID = paste(state_code, county_code, site_number, sep = "-"),
                 pm25               = arithmetic_mean)
        
        # Safely determine local_site_name
        if (!"local_site_name" %in% names(filt)) {
          filt$local_site_name <- if ("city" %in% names(filt)) filt$city else 
                                  if ("city_name" %in% names(filt)) filt$city_name else "Unknown Site"
        } else {
          # Handle cases where local_site_name might be NA or empty
          filt$local_site_name <- if_else(!is.na(filt$local_site_name) & filt$local_site_name != "", 
                                          filt$local_site_name, 
                                          if ("city" %in% names(filt)) filt$city else 
                                          if ("city_name" %in% names(filt)) filt$city_name else "Unknown Site")
        }
      } else {
        # Use fetch_airnow_daily with states parameter
        filt <- fetch_airnow_daily(input$aqs_dates[1], input$aqs_dates[2], states = input$aqs_state)
        # Standardize column names if needed to match AQS output
        if (nrow(filt) > 0) {
          filt <- filt %>% rename(local_site_name = City, latitude = Latitude, longitude = Longitude)
        }
      }
      if (nrow(filt) == 0) { showNotification("No PM2.5 data found.", type = "error"); return() }

      sites_sf <- filt %>%
        distinct(Monitoring_Site_ID, latitude, longitude, local_site_name) %>%
        st_as_sf(coords = c("longitude","latitude"), crs = 4326)

      setProgress(0.5, detail = "Fetching HMS Smoke per day...")
      d_range     <- seq(input$aqs_dates[1], input$aqs_dates[2], by = "day")
      smoke_combo <- map_dfr(d_range, function(d) {
        s <- fetch_hms_smoke_day(d)
        if (is.null(s) || nrow(s) == 0) return(NULL)
        if (is.na(st_crs(s))) s <- st_set_crs(s, 4326)
        if (is.na(st_crs(s))) s <- st_set_crs(s, 4326)
        
        # Use distance-based check if buffer > 0
        if (input$aqs_buffer > 0) {
          # Transform to planar for accurate meter distance
          s_planar     <- st_transform(s, 3857)
          sites_planar <- st_transform(sites_sf, 3857)
          
          # Calculate distances to nearest smoke polygon
          dists <- st_distance(sites_planar, s_planar)
          # Get the minimum distance for each site to any polygon in 's'
          min_dists <- apply(dists, 1, min)
          
          # Filter sites within buffer (convert km to m)
          impacted_sites <- sites_sf[as.numeric(min_dists) <= (input$aqs_buffer * 1000), ]
          
          if (nrow(impacted_sites) == 0) return(NULL)
          
          # Since st_distance doesn't join attributes, we need to find which polygon was closest to get Density
          nearest_idx <- apply(dists, 1, which.min)
          impacted_sites$Density <- s$Density[nearest_idx[as.numeric(min_dists) <= (input$aqs_buffer * 1000)]]
          
          int <- impacted_sites
        } else {
          # Strict intersection
          int <- suppressWarnings(try(st_intersection(s, sites_sf), silent = TRUE))
        }

        if (inherits(int, "try-error") || nrow(int) == 0) return(NULL)
        pm_d <- filt %>% filter(date == d) %>% select(Monitoring_Site_ID, pm25)
        int %>%
          left_join(pm_d, by = "Monitoring_Site_ID") %>%
          mutate(date = d, Smoke_Intensity = str_to_title(Density)) %>%
          select(Monitoring_Site_ID, local_site_name, Smoke_Intensity, date, pm25) %>%
          filter(!is.na(date)) %>%
          st_drop_geometry()
      })

      if (nrow(smoke_combo) == 0) {
        showNotification("No smoke impacts found.", type = "error"); return()
      }

      eoc <- smoke_combo %>%
        filter(pm25 >= input$aqs_thresh, Smoke_Intensity %in% input$aqs_smoke_filt)
      aqs_rv$events <- eoc

      # Consolidated Plot: Full Summary with Highlights for High Events
      p_sum <- ggplotly(
        ggplot(smoke_combo %>% filter(!is.na(local_site_name)),
               aes(x = date, y = pm25, color = Smoke_Intensity,
                   text = paste0("Site: ", local_site_name,
                                 "<br>Date: ", date,
                                 "<br>PM2.5: ", round(pm25,1), " \u00b5g/m\u00b3",
                                 "<br>Smoke: ", Smoke_Intensity))) +
          # Show all points
          geom_point(aes(size = pm25 >= input$aqs_thresh), alpha = 0.7) +
          # Add labels ONLY for points above threshold
          geom_text(data = smoke_combo %>% filter(pm25 >= input$aqs_thresh),
                    aes(label = round(pm25,1)), vjust = -1.5, size = 2.5,
                    color = "black", show.legend = FALSE) +
          scale_color_manual(values = c(Light="gray70", Medium="darkorange", Heavy="firebrick")) +
          scale_size_manual(values = c("FALSE" = 2.0, "TRUE" = 3.5), guide = "none") +
          facet_wrap(~local_site_name, scales = "free_y") + 
          theme_minimal() +
          labs(title = paste("Smoke Impact Summary (Highlights >", input$aqs_thresh, "\u00b5g/m\u00b3)"),
               x = "Date", y = "PM2.5 \u00b5g/m\u00b3"),
        tooltip = "text"
      )
      aqs_rv$summary_p <- p_sum
    })
  })

  output$aqs_summary_plot <- renderPlotly({ req(aqs_rv$summary_p);  aqs_rv$summary_p })
  output$aqs_event_tbl    <- renderDT({
    req(aqs_rv$events)
    datatable(aqs_rv$events, options = list(pageLength = 15, scrollX = TRUE))
  })
  output$dl_aqs_events <- downloadHandler("smoke_events.csv",
    function(f) write.csv(aqs_rv$events, f, row.names = FALSE))

  # ==========================================================
  # TAB 4 — Single Day PM2.5 Map
  # ==========================================================
  sd_rv <- reactiveValues(map_data = NULL, summary = NULL)

  observeEvent(input$run_sd, {
    req(input$sd_date, input$sd_state)
    withProgress(message = "Building PM2.5 map...", value = 0.3, {
      d <- as.Date(input$sd_date)
      st_code <- state_name_to_code[input$sd_state]
      
      if (input$sd_src == "aqs") {
        req(input$sd_email, input$sd_key)
        myu <- create_user(email = input$sd_email, key = input$sd_key)
        yr  <- year(d)
        # Fetching by state code
        raw <- aqs_dailyData(myu, "byState", "88101",
                             paste0(yr,"0101"), paste0(yr,"1231"), state = st_code)
        filt <- raw %>%
          mutate(date = as.Date(date_local)) %>%
          filter(date == d) %>%
          mutate(Monitoring_Site_ID = paste(state_code, county_code, site_number, sep = "-"),
                 pm25 = arithmetic_mean)
        
        # Safe column extraction for AQS
        if (!"City" %in% names(filt)) {
          filt$City <- if ("local_site_name" %in% names(filt)) filt$local_site_name else 
                       if ("city" %in% names(filt)) filt$city else 
                       if ("city_name" %in% names(filt)) filt$city_name else "Unknown City"
        }
        if (!"County" %in% names(filt)) {
          filt$County <- if ("county" %in% names(filt)) filt$county else 
                         if ("county_name" %in% names(filt)) filt$county_name else "Unknown County"
        }
      } else {
        # Using the base fetch_airnow_daily function with the state name filter
        filt <- fetch_airnow_daily(d, d, states = input$sd_state)
        # Add County/City if missing (AirNow daily file format v2 varies)
        if (nrow(filt) > 0) {
          if (!"County" %in% names(filt)) filt$County <- "N/A"
          if (!"City" %in% names(filt)) filt$City <- "Unknown"
        }
      }
      
      if (nrow(filt) == 0) { showNotification("No PM2.5 data found.", type = "error"); return() }

      setProgress(0.5, detail = "Fetching smoke...")
      smoke_sf <- fetch_hms_smoke_day(d)
      fires_sf <- if (input$sd_fires) fetch_hms_fires_day(d) else NULL

      # Instead of joining with ms_sites, we use the sites from the data itself
      pm_sites <- filt %>%
        filter(!is.na(pm25)) %>%
        mutate(AQI_Cat = aqi_cat(pm25),
               label   = paste0("<b>", City, "</b><br>", County, " Co.<br>PM2.5: <b>",
                                round(pm25, 1), "</b> \u00b5g/m\u00b3<br>", AQI_Cat))

      sd_rv$map_data <- list(pm_sites = pm_sites, smoke_sf = smoke_sf,
                             fires_sf = fires_sf, date = d)
      sd_rv$summary  <- pm_sites %>% select(City, County, pm25, AQI_Cat) %>% arrange(desc(pm25))
    })
  })

  output$sd_map <- renderLeaflet({
    req(sd_rv$map_data)
    md <- sd_rv$map_data
    pm <- md$pm_sites
    pal <- colorFactor(
      palette = c("#00e400","#ffff00","#ff7e00","#ff0000","#8f3f97","#7e0023"),
      levels  = c("Good","Moderate","Unhealthy for Sensitive Groups",
                  "Unhealthy","Very Unhealthy","Hazardous"),
      na.color = "gray")

    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron",  group = "Basemap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite")

    # 1. Add Smoke Polygons First (Bottom)
    if (!is.null(md$smoke_sf) && nrow(md$smoke_sf) > 0) {
      sp <- colorFactor(c("#D3D3D3","#808080","#404040"),
                        levels = c("Light","Medium","Heavy"), na.color = "transparent")
      m <- m %>%
        addPolygons(data = md$smoke_sf, fillColor = ~sp(Density),
                    fillOpacity = 0.5, color = "#555", weight = 0.8,
                    popup = ~paste0("HMS Smoke: ", Density), group = "HMS Smoke") %>%
        addLegend("bottomleft", pal = sp, values = c("Light","Medium","Heavy"),
                  title = "HMS Smoke Density", group = "HMS Smoke")
    }
    
    # 2. Add HMS Fires
    if (input$sd_fires && !is.null(md$fires_sf) && nrow(md$fires_sf) > 0) {
      fc <- st_coordinates(md$fires_sf)
      m <- m %>%
        addCircleMarkers(lng = fc[,1], lat = fc[,2], radius = 3,
                         color = "#e74c3c", fillOpacity = 0.7, stroke = FALSE,
                         popup = "HMS Fire", group = "HMS Fires")
    }

    # 3. Add PM2.5 Monitors Last (Top)
    if (nrow(pm) > 0) {
      m <- m %>%
        addCircleMarkers(data = pm, lng = ~Longitude, lat = ~Latitude,
                         radius = 12, color = "black", weight = 1,
                         fillColor = ~pal(AQI_Cat), fillOpacity = 0.9,
                         popup = ~label,
                         label = ~paste0(City, ": ", round(pm25,1), " \u00b5g/m\u00b3"),
                         group = "PM2.5 Monitors") %>%
        addLegend("bottomright", pal = pal,
                  values = c("Good","Moderate","Unhealthy for Sensitive Groups",
                             "Unhealthy","Very Unhealthy","Hazardous"),
                  title  = paste("PM2.5 AQI \u2014", md$date))
      
      # Dynamically fit bounds to the monitoring sites
      m <- m %>% fitBounds(lng1 = min(pm$Longitude), lat1 = min(pm$Latitude),
                           lng2 = max(pm$Longitude), lat2 = max(pm$Latitude))
    } else {
      # Fallback centering if no monitors
      m <- m %>% setView(lng = -98.5, lat = 39.5, zoom = 4)
    }

    m %>% addLayersControl(
      baseGroups    = c("Basemap","Satellite"),
      overlayGroups = c("HMS Smoke","HMS Fires","PM2.5 Monitors"),
      options       = layersControlOptions(collapsed = FALSE))
  })

  output$sd_map_title <- renderUI({
    state_name <- if (!is.null(input$sd_state)) input$sd_state else "Mississippi"
    HTML(paste("Interactive PM2.5 Map \u2014", state_name, "Monitors"))
  })

  output$sd_value_boxes <- renderUI({
    req(sd_rv$summary)
    s <- sd_rv$summary
    tagList(
      hr(),
      h6(paste("Summary \u2014", sd_rv$map_data$date)),
      tags$table(class = "table table-sm table-striped",
        tags$thead(tags$tr(tags$th("Site"), tags$th("PM2.5"), tags$th("AQI"))),
        tags$tbody(lapply(seq_len(nrow(s)), function(i)
          tags$tr(tags$td(s$City[i]), tags$td(round(s$pm25[i],1)), tags$td(s$AQI_Cat[i]))
        ))
      )
    )
  })

  # ==========================================================
  # TAB 5 — Smoke Layer Analysis
  # ==========================================================
  sl_rv <- reactiveValues(
    fire_prox   = NULL, fires_sf  = NULL, pt       = NULL,
    smoke_sf    = NULL,   # full national smoke — for map display
    smoke_local = NULL,   # 50 km buffer clip  — for assessment text
    hrrr        = NULL, pm_nearby = NULL, pm_daily = NULL,
    altitude    = NULL,
    # --- map-click sampling ---
    clicked_pt       = NULL,   # sf point of last click
    clicked_coords   = NULL,   # c(lon=, lat=) numeric
    altitude_click   = NULL,   # assess_smoke_altitude() result at click
    smoke_at_click   = NULL,   # HMS smoke density string at click
    fire_dist_click  = NULL    # nearest fire km from click
  )

  observeEvent(input$run_sl, {
    req(input$sl_date, input$sl_location)
    withProgress(message = "Running smoke layer analysis...", value = 0, {

      incProgress(0.1, detail = "Geocoding...")
      loc <- suppressWarnings(geocode(tibble(a = input$sl_location),
                                      address = "a", method = "osm", quiet = TRUE))
      if (is.na(loc$lat)) { showNotification("Geocoding failed.", type = "error"); return() }
      pt       <- st_as_sf(loc, coords = c("long","lat"), crs = 4326)
      pt_coords <- st_coordinates(pt)
      sl_rv$pt <- pt

      incProgress(0.1, detail = "Fetching HMS Fires...")
      f1_sl <- fetch_hms_fires_day(input$sl_date)
      f2_sl <- fetch_hms_fires_day(input$sl_date + 1)
      fires_sf <- bind_rows(f1_sl, f2_sl)
      sl_rv$fires_sf <- fires_sf
      sl_rv$fire_prox <- if (!is.null(fires_sf) && nrow(fires_sf) > 0) calc_fire_proximity(fires_sf, pt, input$sl_radius) else NULL

      incProgress(0.1, detail = "Fetching HMS Smoke...")
      s1_sl <- fetch_hms_smoke_day(input$sl_date)
      s2_sl <- fetch_hms_smoke_day(input$sl_date + 1)
      smoke_sf <- bind_rows(s1_sl, s2_sl)
      # Store full national smoke for the map display (same coverage as Tab 4)
      sl_rv$smoke_sf <- smoke_sf
      # Separately clip to a small buffer around the monitor for the assessment text
      # (did smoke actually pass over this location?)
      buf_local <- pt %>% st_transform(5070) %>%
        st_buffer(50 * 1000) %>% st_transform(4326)   # 50 km local buffer
      sl_rv$smoke_local <- if (!is.null(smoke_sf) && nrow(smoke_sf) > 0)
        suppressWarnings(st_filter(smoke_sf, buf_local, .predicate = st_intersects))
      else
        NULL

      incProgress(0.1, detail = "Fetching nearby AirNow hourly...")
      buf_pm  <- pt %>% st_transform(5070) %>% st_buffer(100 * 1000) %>% st_transform(4326)
      pm_raw  <- fetch_airnow_hourly(input$sl_date, input$sl_date + 1)
      if (nrow(pm_raw) > 0) {
        # Apply local filtering to avoid UTC-shift artifacts
        tz_sl <- get_olson_tz(as.integer(input$sl_gmt_offset))
        pm_raw_local <- pm_raw %>%
          mutate(DateTimeGMT   = mdy_hm(paste(ValidDate, sprintf("%02d:00", Hour_UTC)), tz = "GMT"),
                 DateTimeLocal = with_tz(DateTimeGMT, tz_sl)) %>%
          filter(DateTimeLocal >= as.POSIXct(paste(input$sl_date, "00:00:00"), tz = tz_sl),
                 DateTimeLocal <= as.POSIXct(paste(input$sl_date, "23:59:59"), tz = tz_sl))
        
        if (nrow(pm_raw_local) > 0) {
          pm_sf <- st_as_sf(pm_raw_local, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)
          sl_rv$pm_nearby <- st_drop_geometry(st_filter(pm_sf, buf_pm, .predicate = st_intersects))
        } else {
          sl_rv$pm_nearby <- data.frame()
        }
      }

      # FIX v3: use a ±5° bbox so we don't download all-US daily data
      incProgress(0.15, detail = "Fetching nearby AirNow daily...")
      daily_bbox <- c(
        xmin = unname(pt_coords[1,1]) - 5, ymin = unname(pt_coords[1,2]) - 5,
        xmax = unname(pt_coords[1,1]) + 5, ymax = unname(pt_coords[1,2]) + 5
      )
      sl_rv$pm_daily <- fetch_airnow_daily(input$sl_date, input$sl_date, bbox = daily_bbox)

      incProgress(0.2, detail = "Fetching HRRR-Smoke...")
      hrrr_bbox <- c(
        xmin = unname(pt_coords[1,1]) - 3, ymin = unname(pt_coords[1,2]) - 3,
        xmax = unname(pt_coords[1,1]) + 3, ymax = unname(pt_coords[1,2]) + 3
      )
      hrrr          <- fetch_hrrr_smoke(input$sl_date, input$sl_hrrr_hour, hrrr_bbox)
      sl_rv$hrrr    <- hrrr
      # NEW v3: compute altitude verdict from COLMD vs MASSDEN at monitor location
      sl_rv$altitude <- assess_smoke_altitude(hrrr, pt)
    })
  })

  output$sl_fire_map <- renderLeaflet({
    req(sl_rv$pt)
    pt_c <- st_coordinates(sl_rv$pt)
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = unname(pt_c[1,1]), lat = unname(pt_c[1,2]), zoom = 5)

    ring_kms  <- c(100, 250, 500)
    ring_cols <- c("#3498db","#e67e22","#e74c3c")
    for (i in seq_along(ring_kms)) {
      ring_sf <- sl_rv$pt %>% st_transform(5070) %>%
        st_buffer(ring_kms[i] * 1000) %>% st_transform(4326)
      m <- m %>%
        addPolygons(data = ring_sf, fill = FALSE, color = ring_cols[i],
                    weight = 1.5, dashArray = "5",
                    label  = paste0(ring_kms[i], " km radius"))
    }
    if (!is.null(sl_rv$smoke_sf) && nrow(sl_rv$smoke_sf) > 0) {
      sp <- colorFactor(c("#D3D3D3","#808080","#404040"),
                        levels = c("Light","Medium","Heavy"), na.color = "transparent")
      m <- m %>%
        addPolygons(data = sl_rv$smoke_sf, fillColor = ~sp(Density),
                    fillOpacity = 0.4, color = "#555", weight = 0.5,
                    popup = ~paste("Smoke:", Density), group = "HMS Smoke")
    }
    if (!is.null(sl_rv$fires_sf) && nrow(sl_rv$fires_sf) > 0) {
      fc      <- st_coordinates(sl_rv$fires_sf)
      fd      <- st_drop_geometry(sl_rv$fires_sf)
      pop_txt <- if ("FRP" %in% names(fd)) paste0("FRP: ", fd$FRP) else "HMS Fire"
      m <- m %>%
        addCircleMarkers(lng = fc[,1], lat = fc[,2], radius = 3,
                         color = "#e74c3c", fillOpacity = 0.7, stroke = FALSE,
                         popup = pop_txt, group = "HMS Fires")
    }
    if (!is.null(sl_rv$pm_daily) && nrow(sl_rv$pm_daily) > 0) {
      pal_d <- colorFactor(c("#00e400","#ffff00","#ff7e00","#ff0000","#8f3f97","#7e0023"),
                           levels  = c("Good","Moderate","Unhealthy for Sensitive Groups",
                                       "Unhealthy","Very Unhealthy","Hazardous"),
                           na.color = "#808080")
      pd <- sl_rv$pm_daily
      m <- m %>%
        addCircleMarkers(data = pd, lng = ~Longitude, lat = ~Latitude, radius = 4,
                         color = "#333333", weight = 1,
                         fillColor = ~pal_d(AQI_Cat), fillOpacity = 0.8,
                         group  = "AirNow Daily PM2.5",
                         popup  = ~paste0("<b>",City,"</b><br>PM2.5: ",round(pm25,1),
                                          " \u00b5g/m\u00b3<br>AQI: ",AQI_Cat)) %>%
        addLegend("bottomleft",
                  colors  = c("#00e400","#ffff00","#ff7e00","#ff0000","#8f3f97","#7e0023"),
                  labels  = c("Good","Moderate","USG","Unhealthy","Very Unhealthy","Hazardous"),
                  title   = paste("Daily PM2.5 \u2014", input$sl_date), opacity = 0.8)
    }
    m %>%
      addCircleMarkers(lng = unname(pt_c[1,1]), lat = unname(pt_c[1,2]),
                       radius = 10, color = "#2c3e50", fillColor = "#2ecc71",
                       fillOpacity = 1, popup = input$sl_location) %>%
      addLayersControl(overlayGroups = c("HMS Smoke","HMS Fires","AirNow Daily PM2.5"),
                       options = layersControlOptions(collapsed = FALSE))
  })

  output$sl_hrrr_plot <- renderPlotly({
    req(sl_rv$hrrr)
    h <- sl_rv$hrrr
    if (!is.null(h$surface) && requireNamespace("terra", quietly = TRUE)) {
      surf <- terra::project(h$surface, "EPSG:4326")
      surf <- surf * 1e9
      df   <- as.data.frame(surf, xy = TRUE)
      names(df)[3] <- "MASSDEN"

      states_map <- map_data("state")

      # Threshold at 1 µg/m³
      df_smoke <- df[!is.na(df$MASSDEN) & df$MASSDEN >= 1.0, ]

      upper_lim <- if (nrow(df_smoke) > 0)
        max(10, min(quantile(df_smoke$MASSDEN, 0.99, na.rm = TRUE), 150))
      else 60

      p <- ggplot()

      if (nrow(df_smoke) > 0) {
        p <- p + 
          geom_tile(data = df_smoke, aes(x = x, y = y, fill = MASSDEN, 
                                        text = paste0("Lon: ", round(x, 2), "<br>Lat: ", round(y, 2), 
                                                      "<br>Smoke: ", round(MASSDEN, 1), " \u00b5g/m\u0033")), 
                    alpha = 0.9, width = 0.05, height = 0.05) +
          scale_fill_gradientn(
            colors   = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026","#67000d"),
            name     = "Smoke (\u00b5g/m\u00b3)",
            trans    = "log10",
            limits   = c(1, upper_lim),
            breaks   = c(1, 5, 10, 25, 50),
            oob      = scales::squish,
            na.value = "transparent"
          )
      } else {
        p <- p +
          annotate("text",
                   x     = mean(range(df$x, na.rm = TRUE)),
                   y     = mean(range(df$y, na.rm = TRUE)),
                   label = "No significant near-surface smoke detected\n(\u22651 \u00b5g/m\u00b3 threshold)",
                   color = "#444444", size = 4)
      }
      
      # Add state outlines ON TOP but HOLLOW (fill = NA)
      p <- p + 
        geom_polygon(data = states_map, aes(x = long, y = lat, group = group),
                     fill = NA, color = "#666666", linewidth = 0.3) +
        theme_void() + coord_fixed(1.3, xlim = range(df$x), ylim = range(df$y))
      
      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(l = 0, r = 0, b = 0, t = 40))
    }
  })

  output$sl_hrrr_msg <- renderText({ req(sl_rv$hrrr); sl_rv$hrrr$msg })

  # ---- Map click: sample HRRR + HMS at any clicked location ----
  observeEvent(input$sl_fire_map_click, {
    click <- input$sl_fire_map_click
    if (is.null(click)) return()

    clicked_pt <- st_as_sf(
      data.frame(lon = click$lng, lat = click$lat),
      coords = c("lon", "lat"), crs = 4326
    )
    sl_rv$clicked_pt     <- clicked_pt
    sl_rv$clicked_coords <- c(lon = click$lng, lat = click$lat)

    # HRRR MASSDEN + COLMD at clicked pixel (requires HRRR already fetched)
    sl_rv$altitude_click <- if (!is.null(sl_rv$hrrr))
      assess_smoke_altitude(sl_rv$hrrr, clicked_pt)
    else
      list(surface_ug = NA, column_mg = NA,
           verdict = "Run the analysis first to load HRRR data.")

    # HMS smoke density at clicked point (polygon intersection)
    sl_rv$smoke_at_click <- tryCatch({
      if (!is.null(sl_rv$smoke_sf) && nrow(sl_rv$smoke_sf) > 0) {
        int <- suppressWarnings(st_intersection(sl_rv$smoke_sf, clicked_pt))
        if (nrow(int) > 0)
          paste(sort(unique(int$Density)), collapse = " / ")
        else
          "None (outside all HMS smoke polygons)"
      } else "No HMS smoke loaded"
    }, error = function(e) "Unable to determine")

    # Nearest fire distance from clicked point
    sl_rv$fire_dist_click <- tryCatch({
      if (!is.null(sl_rv$fires_sf) && nrow(sl_rv$fires_sf) > 0) {
        fp <- calc_fire_proximity(sl_rv$fires_sf, clicked_pt, 2000)
        paste0(fp$nearest_km, " km (", fp$nearest_bearing, ")")
      } else "No HMS fires loaded"
    }, error = function(e) "N/A")

    # Drop an orange marker on the map at the clicked point
    leafletProxy("sl_fire_map") %>%
      clearGroup("Click Marker") %>%
      addCircleMarkers(
        lng         = click$lng,
        lat         = click$lat,
        radius      = 9,
        color       = "#f39c12",
        fillColor   = "#f39c12",
        fillOpacity = 0.85,
        stroke      = TRUE,
        weight      = 2.5,
        label       = paste0(round(click$lat, 3), "\u00b0N  ",
                             abs(round(click$lng, 3)), "\u00b0W"),
        labelOptions = labelOptions(permanent = TRUE, direction = "top",
                                    style = list("font-weight" = "bold",
                                                 "font-size"   = "11px")),
        group = "Click Marker"
      )
  })

  # NEW v3: HRRR altitude verdict card
  output$sl_altitude_ui <- renderUI({
    req(sl_rv$altitude)

    # Helper: build one verdict alert + data table for any location
    make_altitude_block <- function(alt, loc_label, hms_density = NULL,
                                    fire_dist = NULL, coords = NULL) {
      verdict   <- alt$verdict
      box_class <- if      (grepl("ALOFT",       verdict)) "info"
                   else if (grepl("AT SURFACE",  verdict)) "danger"
                   else if (grepl("NEAR-SURFACE",verdict)) "warning"
                   else "secondary"

      surf_lbl <- if (!is.na(alt$surface_ug)) paste0(alt$surface_ug, " \u00b5g/m\u00b3") else "N/A"
      col_lbl  <- if (!is.na(alt$column_mg))  paste0(alt$column_mg,  " mg/m\u00b2")       else "N/A"

      coord_lbl <- if (!is.null(coords))
        paste0(round(coords["lat"], 3), "\u00b0N  ", abs(round(coords["lon"], 3)), "\u00b0W")
      else loc_label

      tagList(
        tags$h6(tags$b(loc_label),
                if (!is.null(coords))
                  tags$small(class = "text-muted ms-2", coord_lbl)),
        div(class = paste0("alert alert-", box_class),
            style = "font-size:0.85em; padding:6px 10px; margin-bottom:6px;",
            tags$b(verdict)),
        tags$table(class = "table table-sm table-bordered mb-1",
          tags$tbody(
            tags$tr(tags$th("MASSDEN (8 m surface)"), tags$td(surf_lbl)),
            tags$tr(tags$th("COLMD (entire column)"),  tags$td(col_lbl)),
            if (!is.null(hms_density))
              tags$tr(tags$th("HMS Smoke density"),    tags$td(hms_density)),
            if (!is.null(fire_dist))
              tags$tr(tags$th("Nearest HMS fire"),     tags$td(fire_dist))
          )
        )
      )
    }

    # --- Monitor location block ---
    mon_coords <- if (!is.null(sl_rv$pt)) {
      c_xy <- st_coordinates(sl_rv$pt)
      c(lon = unname(c_xy[1,1]), lat = unname(c_xy[1,2]))
    } else NULL

    mon_hms <- if (!is.null(sl_rv$smoke_local) && nrow(sl_rv$smoke_local) > 0)
      paste(sort(unique(sl_rv$smoke_local$Density)), collapse = " / ")
    else "None over monitor"

    mon_fire <- if (!is.null(sl_rv$fire_prox) && !is.na(sl_rv$fire_prox$nearest_km))
      paste0(sl_rv$fire_prox$nearest_km, " km (", sl_rv$fire_prox$nearest_bearing, ")")
    else "N/A"

    monitor_block <- make_altitude_block(
      sl_rv$altitude,
      loc_label   = paste0("\U0001f7e2  Monitor \u2014 ", input$sl_location),
      hms_density = mon_hms,
      fire_dist   = mon_fire,
      coords      = mon_coords
    )

    # --- Clicked location block (only if a click has happened) ---
    click_block <- if (!is.null(sl_rv$altitude_click)) {
      tagList(
        tags$hr(style = "margin:8px 0;"),
        make_altitude_block(
          sl_rv$altitude_click,
          loc_label   = "\U0001f7e0  Clicked Location",
          hms_density = sl_rv$smoke_at_click,
          fire_dist   = sl_rv$fire_dist_click,
          coords      = sl_rv$clicked_coords
        )
      )
    } else {
      tagList(
        tags$hr(style = "margin:8px 0;"),
        helpText(
          tags$i(class = "text-muted",
            "\U0001f4cd Click anywhere on the Fire Locations map to sample",
            " HRRR smoke values at that point.")
        )
      )
    }

    tagList(
      monitor_block,
      click_block,
      tags$hr(style = "margin:8px 0;"),
      helpText(style = "font-size:0.78em; color:#666;",
        "MASSDEN: near-surface smoke density (\u00b5g/m\u00b3). ",
        "COLMD: column-integrated smoke (mg/m\u00b2). ",
        "High COLMD + low MASSDEN \u2192 smoke aloft. ",
        "HRRR run: ", paste(input$sl_date, sprintf("%02d UTC", input$sl_hrrr_hour)), ".")
    )
  })

  output$sl_assessment <- renderText({
    req(sl_rv$fire_prox)
    fp        <- sl_rv$fire_prox
    # Use smoke_local (50 km clip) — did smoke actually pass over the monitor?
    dens_levs <- if (!is.null(sl_rv$smoke_local) && nrow(sl_rv$smoke_local) > 0)
      unique(sl_rv$smoke_local$Density) else character()
    pm_vals   <- if (!is.null(sl_rv$pm_nearby) && nrow(sl_rv$pm_nearby) > 0)
      sl_rv$pm_nearby$PM25 else NULL
    smoke_impact_text(fp$nearest_km, fp$within_100, dens_levs, pm_vals)
  })

  output$sl_fire_tbl <- renderDT({
    req(sl_rv$fire_prox)
    fp <- sl_rv$fire_prox
    if (!is.null(fp$fires_df))
      datatable(fp$fires_df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$hysplit_link_ui <- renderUI({
    pt_c <- if (!is.null(sl_rv$pt)) st_coordinates(sl_rv$pt) else matrix(c(-90, 32.5), nrow = 1)
    tagList(
      p(tags$b("Manual HYSPLIT Parameters:")),
      tags$ul(
        tags$li("Lat: ", round(pt_c[1,2], 3), " | Lon: ", round(pt_c[1,1], 3)),
        tags$li("Date: ", as.character(input$sl_date)),
        tags$li("Run Duration: -48 hrs (backward)"),
        tags$li("Heights: 500 m, 1500 m, 3000 m AGL")
      ),
      tags$a(href = "https://www.ready.noaa.gov/HYSPLIT.php",
             target = "_blank", class = "btn btn-outline-secondary btn-sm w-100",
             "Open HYSPLIT on NOAA ARL")
    )
  })

  # ==========================================================
  # TAB 6 — Monitor Data Prep
  # ==========================================================
  prep_rv <- reactiveValues(status = "Awaiting file upload...", data = NULL)

  observeEvent(input$run_prep, {
    req(input$site_dat)
    withProgress(message = "Parsing .dat file...", value = 1, {
      con   <- file(input$site_dat$datapath, "rb")
      lines <- readLines(con, encoding = "UTF-8", warn = FALSE)
      close(con)
      dl <- lapply(lines, function(l) {
        f <- unlist(str_split(l, "\\|"))
        if (length(f) < 22) f <- c(f, rep(NA, 22 - length(f)))
        data.frame(
          AQSID          = f[1],  Parameter_name = f[2], Site_code = f[3],
          Site_name      = f[4],  Status         = f[5], Agency_ID = f[6],
          Agency_name    = f[7],  EPA_region     = f[8],
          Latitude       = suppressWarnings(as.numeric(f[9])),
          Longitude      = suppressWarnings(as.numeric(f[10])),
          Elevation      = suppressWarnings(as.numeric(f[11])),
          GMT_offset     = suppressWarnings(as.numeric(f[12])),
          MSA_code       = f[16], MSA_name       = trimws(f[17]),
          State_code     = f[18], State_name     = f[19],
          County_code    = f[20], County_name    = f[21],
          stringsAsFactors = FALSE
        )
      })
      m_sites <- do.call(rbind, dl)
      m_sites <- m_sites[complete.cases(m_sites), ]
      m_sites[m_sites == ""] <- NA
      m_sites$Monitoring_Site_ID <- paste0(
        m_sites$State_code, "-",
        str_pad(m_sites$County_code, 3, pad = "0"), "-",
        str_pad(m_sites$Site_code,   4, pad = "0")
      )
      m_sites <- m_sites %>% filter(Parameter_name == "PM2.5", Status == "Active")
      prep_rv$data   <- m_sites
      prep_rv$status <- paste("Parsed", nrow(m_sites), "Active PM2.5 sites. Ready to download.")
    })
  })

  output$prep_status  <- renderText({ prep_rv$status })
  output$prep_preview <- renderDT({
    req(prep_rv$data)
    datatable(head(prep_rv$data, 50), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$dl_monitor_csv <- downloadHandler("monitoring_site_locations.csv",
    function(f) write_csv(prep_rv$data, f))

}

shinyApp(ui, server)
