A comprehensive R Shiny application for nationwide PM2.5 monitoring and HMS (Hazard Mapping System) smoke analysis. This tool is designed for air quality analysts and regulatory agencies to investigate exceptional events and smoke impacts across the United States.

### 🚀 **Live Application**: [Shiny Smoke Analysis App](https://rcuevas.shinyapps.io/Shiny_Smoke_Analysis/)

## Features

-   **Nationwide Analysis**: Dynamically fetch PM2.5 data for any US state using AirNow and AQS APIs.
-   **Multi-Day Smoke Events**: Identify significant smoke impacts over time with configurable PM2.5 thresholds and proximity buffers.
-   **Interactive Mapping**: Visualize smoke plumes (HMS) and monitor concentrations with a layered, interactive Leaflet map.
-   **Analytical Tools**: Includes diurnal profile analysis (now with full **Local Standard Time** 24-hour support), HMS fire detections, and consolidated reporting tables.
-   **Deduplicated Data**: "Nearest Fires" table now consolidates overlapping detections into a single maximum-intensity record.
-   **Standards-Compliant**: Ozone categorizations updated to match EPA standards (e.g., 0-54 ppb defined as "Good").
-   **Performance & Stability**: Migrated to modern `dplyr` and `ggplot2` syntax for faster processing and cleaner logs.

## Getting Started

### Prerequisites

-   **R** (>= 4.0.0)
-   **RStudio**
-   Required R packages: `shiny`, `leaflet`, `plotly`, `sf`, `dplyr`, `lubridate`, `aqsr`, `bslib`, `DT`, etc.

### Configuration

To use the AQS API features, you will need to set up your credentials. The app looks for the following environment variables or UI inputs:
-   `AQS_EMAIL`
-   `AQS_KEY`

## Installation

1.  Clone this repository.
2.  Open `app.R` in RStudio.
3.  Install missing dependencies using `renv` or `install.packages()`.
4.  Run the app using `shiny::runApp()`.

## Author

**Rodney Cuevas** - *Mississippi Department of Environmental Quality (MDEQ)*
- Email: RCuevas@mdeq.ms.gov

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
