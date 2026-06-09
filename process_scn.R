# ---------------------------------------------------------------------------
# process_scn.R
# Run the FRI densitometer EDITOR pipeline on AK6.SCN (Akarana Road,
# Wharerata Forest — Red Needle Cast study) and write per-core CSV output.
#
# Requires: densitometry.R (in the same directory)
# ---------------------------------------------------------------------------

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

source("densitometry.R")

scn_file   <- "AK6.SCN"
output_dir <- file.path("output", "densitometry")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Run the full pipeline.
#
# All parameters match the Cown & Clement (1983) defaults:
#   ew_lw_threshold = 500 kg/m³   (operator-nominated EW/LW boundary)
#   min_ring_mm     = 2 mm         (minimum plausible ring width)
#   smooth_n        = 5            (5-point running mean before crossing test)
#   air_threshold   = 200 kg/m³    (trim leading air channels below this)
#
# To override ring boundaries for a specific core, pass them via
# manual_boundaries, e.g.:
#   manual_boundaries = list("3" = c(45L, 130L, 210L))
# ---------------------------------------------------------------------------
results <- process_scn(
  filepath          = scn_file,
  ew_lw_threshold   = 500L,
  min_ring_mm       = 2,
  smooth_n          = 5L,
  air_threshold     = 200L,
  manual_boundaries = NULL,
  plot              = TRUE
)

# ---------------------------------------------------------------------------
# Write one CSV per core, matching EDITOR column names.
# ---------------------------------------------------------------------------
for (cid in names(results)) {
  safe_name <- gsub("[^A-Za-z0-9_-]", "_", cid)
  out_path  <- file.path(output_dir, paste0("AK6_core_", safe_name, "_editor.csv"))
  write.csv(results[[cid]]$stats, file = out_path, row.names = FALSE)
  message("Written: ", out_path)
}

# ---------------------------------------------------------------------------
# Combined table — all cores in one file for easy import into the study
# spreadsheet or further R analysis.
# ---------------------------------------------------------------------------
all_stats <- do.call(rbind, lapply(names(results), function(cid) {
  df         <- results[[cid]]$stats
  df$core_id <- cid
  df$scn_file <- basename(scn_file)
  df[, c("scn_file", "core_id", setdiff(names(df), c("scn_file", "core_id")))]
}))

combined_path <- file.path(output_dir, "AK6_all_cores_editor.csv")
write.csv(all_stats, file = combined_path, row.names = FALSE)
message("Combined table written: ", combined_path)
