# ---------------------------------------------------------------------------
# process_scn.R
# Run the FRI densitometer EDITOR pipeline on AK6.SCN (Akarana Road,
# Wharerata Forest — Red Needle Cast study).  Rings are labelled by position
# (no calendar year — that is operator-assigned downstream).  Writes per-core
# and combined CSV output plus annotated PNG plots to output/densitometry/.
#
# Requires: densitometry.R (in the same directory)
# ---------------------------------------------------------------------------

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

source("densitometry.R")

scn_file   <- "AK6.SCN"
output_dir <- file.path("output", "densitometry")
plot_dir   <- file.path(output_dir, "plots")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Run the full pipeline.
#
#   ew_lw_threshold = 500 kg/m3   (operator-nominated EW/LW boundary)
#   min_ring_mm     = 2 mm         (minimum plausible ring width)
#   smooth_n        = 5            (5-point running mean before peak detection)
#   air_threshold   = 200 kg/m3    (trim leading air channels below this)
#   prominence_frac = 0.08         (adaptive latewood-peak prominence cut-off)
#
# To override ring boundaries for a specific core (after reviewing the plot),
# pass channel positions via manual_boundaries, e.g.:
#   manual_boundaries = list("3" = c(45L, 130L, 210L))
# To set a pith ring offset for a core:
#   rings_offset = list("1" = 2L)
# ---------------------------------------------------------------------------
results <- process_scn(
  filepath          = scn_file,
  ew_lw_threshold   = 500L,
  min_ring_mm       = 2,
  smooth_n          = 5L,
  air_threshold     = 200L,
  prominence_frac   = 0.08,
  manual_boundaries = NULL,
  rings_offset      = NULL,
  plot_dir          = plot_dir
)

# ---------------------------------------------------------------------------
# Write one CSV per core (ring-number labelled, mm widths, areas, suspects).
# ---------------------------------------------------------------------------
for (cid in names(results)) {
  safe_name <- gsub("[^A-Za-z0-9_-]", "_", cid)
  out_path  <- file.path(output_dir, paste0("AK6_core_", safe_name, "_editor.csv"))
  write.csv(results[[cid]]$stats, file = out_path, row.names = FALSE)
  message("Written: ", out_path)
}

# ---------------------------------------------------------------------------
# Combined table — all cores in one file.
# ---------------------------------------------------------------------------
all_stats <- do.call(rbind, lapply(names(results), function(cid) {
  df          <- results[[cid]]$stats
  df$core_id  <- cid
  df$scn_file <- basename(scn_file)
  df[, c("scn_file", "core_id", setdiff(names(df), c("scn_file", "core_id")))]
}))

combined_path <- file.path(output_dir, "AK6_all_cores_editor.csv")
write.csv(all_stats, file = combined_path, row.names = FALSE)
message("Combined table written: ", combined_path)
message("Annotated plots written to: ", plot_dir)
