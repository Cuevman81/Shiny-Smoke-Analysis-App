# deploy.R — one-step publish to shinyapps.io
# Usage:  from within the Smoke_App folder, run:  source("deploy.R")
#
# KNOWN ISSUE (2026-06-28): the lockfile pins terra 1.9-34, which was released
# 2026-06-18 and does NOT yet have a prebuilt Linux binary on Posit Package
# Manager. shinyapps.io (GDAL 3.4.1) therefore tries to COMPILE it from source
# and fails (terra 1.9-34 needs GDAL >= 3.6). Older terra (<=1.8-93) builds on
# the cloud but won't compile on a modern local GDAL (3.13), so we cannot pin
# down. Resolution: wait until Posit publishes the terra 1.9-34 jammy binary,
# then just re-run source("deploy.R") — no compile, full features.
#   Check readiness:
#     curl -H "User-Agent: R (4.6.0 x86_64-pc-linux-gnu x86_64 linux-gnu)" \
#       https://packagemanager.posit.co/cran/__linux__/jammy/latest/src/contrib/PACKAGES \
#       | grep -A12 '^Package: terra$'
#   When the terra entry shows a "Path:" / "Built:" line (i.e. NeedsCompilation
#   is satisfied by a binary), the cloud build will succeed.
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
  "dv_module.R",           # EE Design Value module (sourced by app.R)
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
