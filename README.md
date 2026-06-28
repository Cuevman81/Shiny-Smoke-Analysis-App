# Air Quality & Smoke Analysis Dashboard (v4)

A comprehensive R Shiny application for nationwide PM2.5 monitoring and HMS (Hazard Mapping System) smoke analysis. This tool is designed for air quality analysts and regulatory agencies to investigate exceptional events, smoke impacts, and atmospheric trajectories across the United States.

### 🚀 **Live Application**: [Shiny Smoke Analysis App](https://rcuevas.shinyapps.io/Shiny_Smoke_Analysis/)

---

## 🛠️ System Dependencies

The application relies on several open-source spatial, graphics, and trajectory modeling libraries. Run the commands below corresponding to your operating system to install these dependencies before running the app.

### 🍎 **macOS (via Homebrew)**
```bash
brew install gdal geos proj imagemagick
```

### 🐧 **Ubuntu / Debian**
```bash
sudo apt-get update
sudo apt-get install -y binutils libproj-dev gdal-bin libgdal-dev libgeos-dev liblwgeom-dev libmagick++-dev
```

### 🪟 **Windows**
1. Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) matching your R version.
2. Install GDAL, GEOS, and PROJ via the [OSGeo4W Installer](https://www.osgeo.org/projects/osgeo4w/).
3. Download and install [ImageMagick](https://imagemagick.org/script/download.php) (check the box to "Install development headers and libraries for C and C++" during installation).

### 🌪️ **HYSPLIT & Trajectory Data**
For the **Back Trajectory** tool to run:
- The app uses the `splitr` package which interfaces with NOAA HYSPLIT.
- Meteorological data files (such as GDAS1 or Reanalysis binary files) should be placed in the local `met/` directory (e.g. `met/RP202602.gbl`).
- Note: HYSPLIT is configured to execute in a local temporary directory to dodge OneDrive synchronization locks or institutional network restrictions.

---

## 🔑 Configuration & API Keys

To use AQS API features, you need to configure your EPA AQS credentials:
1. **Sign Up**: Register for an AQS API key by visiting the [EPA AQS Data API Signup](https://aqs.epa.gov/aqsweb/documents/data_api.html#signup).
2. **Environment Variables**: Add your credentials to the `.Renviron` file in this directory:
   ```env
   AQS_EMAIL="your_email@agency.gov"
   AQS_KEY="your_api_key"
   ```
   *Alternatively, you can input these credentials directly within the **Settings** tab in the running app.*

---

## 🗺️ Per-Tab Walkthrough

1. **AirNow MSA Analysis**: Compare hourly PM2.5 concentrations across monitors inside a Metropolitan Statistical Area (MSA) or custom radius. Visualizes interactive hourly time-series and average diurnal profiles with optional HMS smoke plume overlays.
2. **Hourly Cross-Section**: Interrogate hourly smoke density alongside surface PM2.5 readings for a target address/city. Uses local GMT offsets to ensure proper timezone correlation.
3. **Multi-Day Smoke Events**: Identify prolonged periods of smoke exposure using user-defined PM2.5 concentration thresholds and proximity buffers to HMS smoke polygons.
4. **Single Day PM2.5 Map**: Renders an interactive map of daily PM2.5 values across selected states to pinpoint region-wide exceptional events.
5. **Smoke Layer Analysis**: Diagnoses whether smoke is aloft or at the surface. Extracts HRRR column-integrated smoke (COLMD) vs. surface mass density (MASSDEN) at monitor locations to verify plume altitude.
6. **Monitor Data Prep**: A utility to convert raw `monitoring_site_locations.dat` coordinate files into clean, active PM2.5 monitor CSV tables.
7. **HMS Smoke PM2.5 Analysis**: Merges historical AirNow monitoring data with NOAA's HMS Smoke Intensity polygons.
8. **Daily AQ, Met, HMS, and Back Trajectory Analysis**: A multi-faceted dashboard integrating NOAA surface weather charts, upper-air maps (925mb to 300mb), and forward/back trajectory generation via HYSPLIT.
9. **Data Summary**: High-level statistical dashboard reporting data completeness, missing value metrics, and spatial distributions of AQI classes.
10. **Settings**: Adjust API caching, view connectivity status logs, export settings, and modify AQS credentials.

---

## 📊 Customizing MSAs (Metropolitan Statistical Areas)

Metropolitan definitions are externalized to [msa_definitions.csv](file:///Users/rodneycuevas/Library/CloudStorage/OneDrive-MississippiDepartmentofEnvironmentalQuality/Documents/Exceptional%20Events/Tools/Smoke_App/msa_definitions.csv). You can add or modify regions without editing the R source code.

### **CSV Schema**
| Column | Description | Example |
| :--- | :--- | :--- |
| `msa_key` | Key name displayed in the dropdown choice | `Charlotte, NC-SC` |
| `name` | Long descriptive name of the MSA | `Charlotte-Concord, NC-SC MSA` |
| `name_short` | Short identifier for file exports | `Charlotte` |
| `state_code` | Semicolon-separated state postal codes | `NC;SC` |
| `xmin`, `ymin`, `xmax`, `ymax` | Bounding box coordinates for spatial filtering | `-81.2, 35.0, -80.5, 35.6` |
| `gmt_offset` | Local GMT timezone offset | `-5` |
| `target_aqsids` | Semicolon-separated specific AQS monitor IDs (optional) | `371190041;450910006` |

---

## ❓ Troubleshooting

* **Missing `msa_definitions.csv`**: If deleted, the app will automatically regenerate the default CSV file at startup.
* **OneDrive / Network Sync Locks**: If HYSPLIT trajectory runs fail due to file permissions, ensure the app isn't being run inside a folder being actively synced by OneDrive (or verify that execution logs in the console show the local temporary execution directories are created correctly).
* **Package Errors**: If you encounter package version breakages, use `renv::restore()` to restore the environment using the committed `renv.lock` file.
* **Clean Cache**: If API downloads fail or retrieve outdated data, go to the **Settings** tab and click **Clear Cache**.

---

## 👥 Author & License

- **Rodney Cuevas** (Mississippi Department of Environmental Quality) — *RCuevas@mdeq.ms.gov*
- Licensed under the [MIT License](LICENSE).
