#10.06.26 20:21 NZST
# ---------------------------------------------------------------------------
# parity_check.R
# Compare the detector against the operator-edited AK6.DAT and search the best
# per-core prominence_frac. Writes the parity and calibration tables to CSV.
#
# Requires: densitometry.R
# ---------------------------------------------------------------------------

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))   # uncomment when sourcing in RStudio
source("densitometry.R")

scn_file   <- "AK6.SCN"
dat_file   <- "AK6.DAT"
output_dir <- file.path("output", "densitometry")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cores <- parse_scn(scn_file)
dat   <- parse_dat(dat_file)

results <- process_scn(scn_file, prominence_frac = 0.12, plot_dir = NULL)
parity  <- compare_to_dat(results, dat)
write.csv(parity, file.path(output_dir, "AK6_parity_default.csv"),
          row.names = FALSE, na = "")

cal <- calibrate_to_dat(cores, dat)
write.csv(cal, file.path(output_dir, "AK6_calibration.csv"),
          row.names = FALSE, na = "")
