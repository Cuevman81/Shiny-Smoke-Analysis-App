# deploy.R — one-step publish to shinyapps.io
# Usage:  from within the Smoke_App folder, run:  source("deploy.R")
#
# This uploads ONLY the files the app needs at runtime (see appFiles below).
# Everything else in the folder — .Renviron (your AQS key), caches, the large
# .dat files, PDFs, renv/library — is intentionally left OUT of the bundle.
#
# Package versions are rebuilt in the cloud from renv.lock, so you do not need
# to upload the renv/ library or .Rprofile.

# --- Files to publish (whitelist) ------------------------------------------
app_files <- c(
  "app.R",                 # the application
  "msa_definitions.csv",   # MSA dropdown definitions (read at startup)
  "ASCDATA.CFG",           # HYSPLIT config (Back Trajectory tab)
  "SETUP.CFG",             # HYSPLIT config (Back Trajectory tab)
  "www/MDEQ_Logo.png",     # navbar logo
  "renv.lock"              # pins the 147 package versions for the cloud build
)

# --- Safety check: make sure every file exists before deploying ------------
missing <- app_files[!file.exists(app_files)]
if (length(missing) > 0) {
  stop("Cannot deploy — these files are missing:\n  ",
       paste(missing, collapse = "\n  "))
}

# --- Safety check: never ship the secret -----------------------------------
if (".Renviron" %in% app_files) {
  stop("Refusing to deploy: .Renviron must not be in the file list.")
}

message("Deploying ", length(app_files), " files to shinyapps.io ...")

# --- Deploy ----------------------------------------------------------------
rsconnect::deployApp(
  appName  = "Shiny_Smoke_Analysis",
  appFiles = app_files,
  forceUpdate = TRUE   # update the existing app without an interactive prompt
)
