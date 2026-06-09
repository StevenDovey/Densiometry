# ---------------------------------------------------------------------------
# parity_check.R
# Compare the automatic detector against the operator-edited AK6.DAT, and
# search the best per-core prominence_frac.  Writes a parity report to
# output/densitometry/.
#
# Requires: densitometry.R (in the same directory)
# ---------------------------------------------------------------------------

source("densitometry.R")

scn_file   <- "AK6.SCN"
dat_file   <- "AK6.DAT"
output_dir <- file.path("output", "densitometry")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cores <- parse_scn(scn_file)
dat   <- parse_dat(dat_file)

# --- 1. Parity at the default prominence_frac (0.08) ------------------------
results <- process_scn(scn_file, prominence_frac = 0.08, plot_dir = NULL)
parity  <- compare_to_dat(results, dat)
cat("\n=== Parity vs AK6.DAT (prominence_frac = 0.08) ===\n")
print(parity, row.names = FALSE)
cat(sprintf("\nMean |diff|: %.2f rings/core   Mean width RMSE: %.2f mm\n",
            mean(abs(parity$diff)), mean(parity$width_rmse_mm)))
write.csv(parity, file.path(output_dir, "AK6_parity_default.csv"), row.names = FALSE)

# --- 2. Best per-core prominence_frac ---------------------------------------
cal <- calibrate_to_dat(cores, dat)
cat("\n=== Calibrated per-core prominence_frac ===\n")
print(cal, row.names = FALSE)
cat(sprintf("\nExact-count cores after calibration: %d / %d\n",
            sum(cal$n_rings == cal$target_n), nrow(cal)))
write.csv(cal, file.path(output_dir, "AK6_calibration.csv"), row.names = FALSE)
