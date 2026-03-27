# Air Quality & HMS Smoke Analysis App (v4)

A comprehensive R Shiny application for nationwide PM2.5 monitoring and HMS (Hazard Mapping System) smoke analysis. This tool is designed for air quality analysts and regulatory agencies to investigate exceptional events and smoke impacts across the United States.

## Features

-   **Nationwide Analysis**: Dynamically fetch PM2.5 data for any US state using AirNow and AQS APIs.
-   **Multi-Day Smoke Events**: Identify significant smoke impacts over time with configurable PM2.5 thresholds and proximity buffers.
-   **Interactive Mapping**: Visualize smoke plumes (HMS) and monitor concentrations with a layered, interactive Leaflet map.
-   **Analytical Tools**: Includes diurnal profile analysis, HMS fire detections, and consolidated reporting tables.
-   **Data Robustness**: Improved handling of varying API response formats and automated geometry cleaning for spatial data.

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
