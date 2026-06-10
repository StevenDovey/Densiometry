#10.06.26 22:30 NZST
# ---------------------------------------------------------------------------
# process_scn.R
# Run the detection pipeline on AK6.SCN, write per-core and combined ring
# results to CSV, and write annotated PNG plots.
#
# Requires: densitometry.R
# ---------------------------------------------------------------------------

source("densitometry.R")

scn_file   <- "AK6.SCN"
output_dir <- file.path("output", "densitometry")
plot_dir   <- file.path(output_dir, "plots")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

results <- process_scn(
  filepath          = scn_file,
  ew_lw_threshold   = 500L,
  min_ring_mm       = 1,
  smooth_n          = 5L,
  air_threshold     = 200L,
  prominence_frac   = 0.12,
  manual_boundaries = NULL,
  rings_offset      = NULL,
  plot_dir          = plot_dir
)

for (cid in names(results)) {
  safe_name <- gsub("[^A-Za-z0-9_-]", "_", cid)
  out_path  <- file.path(output_dir, paste0("AK6_core_", safe_name, "_editor.csv"))
  write.csv(results[[cid]]$stats, file = out_path, row.names = FALSE, na = "")
}

all_stats <- do.call(rbind, lapply(names(results), function(cid) {
  df          <- results[[cid]]$stats
  df$core_id  <- cid
  df$scn_file <- basename(scn_file)
  df[, c("scn_file", "core_id", setdiff(names(df), c("scn_file", "core_id")))]
}))

write.csv(all_stats, file.path(output_dir, "AK6_all_cores_editor.csv"),
          row.names = FALSE, na = "")
