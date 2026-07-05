# deploy.R — one-step publish to shinyapps.io
# Usage:  from within the Smoke_App folder, run:  source("deploy.R")
#
# KNOWN ISSUE (still true as of 2026-07-05): terra 1.9-34 fails to COMPILE
# against the GDAL 3.4.1 on shinyapps.io's Ubuntu jammy build image, and no
# prebuilt jammy binary exists for it. Older terra can't be installed locally
# either (local GDAL 3.13 is too new for 1.8-x source builds).
#
# RESOLUTION (works today, no waiting): generate the deployment manifest from
# the renv library, pin terra to 1.8-86 (last 1.8-series release — its source
# still builds on the cloud image), and deploy via manifestPath, which uses
# the manifest verbatim. renv.lock stays OUT of the bundle so the server
# builds strictly from the pinned manifest.
#
# When a terra release newer than 1.9-34 fixes old-GDAL builds, set
# TERRA_PIN <- NULL and this deploys the library versions unmodified.
#
# This uploads ONLY the files the app needs at runtime (see app_files).
# Everything else — .Renviron (your AQS key), caches, the large .dat files,
# PDFs, renv/library — is intentionally left OUT of the bundle.

TERRA_PIN <- "1.8-86"

# --- Files to publish (whitelist) ------------------------------------------
app_files <- c(
  "app.R",                 # the application
  "dv_module.R",           # EE Design Value module (sourced by app.R)
  "msa_definitions.csv",   # MSA dropdown definitions (read at startup)
  "ASCDATA.CFG",           # HYSPLIT config (Back Trajectory tab)
  "SETUP.CFG",             # HYSPLIT config (Back Trajectory tab)
  "www/MDEQ_Logo.png"      # navbar logo
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

# --- 1. Manifest from the renv library (lockfile stays out of the bundle) --
message("1/3 Capturing dependencies from the renv library...")
rsconnect::writeManifest(appFiles = app_files, dependencyResolution = "library")

# --- 2. Pin terra ------------------------------------------------------------
if (!is.null(TERRA_PIN)) {
  message("2/3 Pinning terra to ", TERRA_PIN, " in manifest.json...")
  txt <- readLines("manifest.json")
  tline <- grep('"Package": "terra"', txt, fixed = TRUE)
  if (length(tline) == 1) {
    vline <- grep('"Version":', txt)
    vline <- vline[vline > tline][1]
    txt[vline] <- sub('"Version": *"[^"]*"', sprintf('"Version": "%s"', TERRA_PIN), txt[vline])
    writeLines(txt, "manifest.json")
    m <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
    stopifnot(m$packages[["terra"]]$description$Version == TERRA_PIN)
    message("    terra pinned: ", m$packages[["terra"]]$description$Version)
  } else {
    message("    terra not found in manifest — deploying as-is")
  }
} else {
  message("2/3 TERRA_PIN is NULL — deploying library versions unmodified")
}

# --- 3. Deploy ---------------------------------------------------------------
message("3/3 Deploying ", length(app_files), " files to shinyapps.io...")
rsconnect::deployApp(
  manifestPath = "manifest.json",
  appName      = "Shiny_Smoke_Analysis",
  forceUpdate  = TRUE
)
