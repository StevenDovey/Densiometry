#10.06.26 20:21 NZST
# ---------------------------------------------------------------------------
# edit_scn.R
# Open one core for interactive operator editing and write the corrected ring
# table to CSV (the new reference). Click a boundary line to remove it, click a
# gap to add one, click DONE to finish. Runs on a machine with a screen.
#
# Usage: Rscript edit_scn.R "<scn file>" "<core id>"
# Requires: ring_review.R, densitometry.R
# ---------------------------------------------------------------------------

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))   # uncomment when sourcing in RStudio
source("ring_review.R")

args    <- commandArgs(trailingOnly = TRUE)
scn     <- args[1]
core_id <- args[2]
out_dir <- file.path("output", "densitometry", "edited")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cores <- parse_scn(scn)
core  <- cores[[core_id]]
d     <- trim_air_channels(core$density, 200L)

dev.new(noRStudioGD = TRUE)
on.exit(dev.off(), add = TRUE)
title <- sprintf("%s  core %s", sub("\\.[^.]*$", "", basename(scn)), core_id)
b0  <- detect_ring_boundaries(d, step_mm = core$step_mm)
cls <- classify_and_infill(d, b0, step_mm = core$step_mm)
est <- sort(unique(c(cls$estimated, estimate_artifact_gaps(d, b0, step_mm = core$step_mm))))
b1  <- edit_core(d, b0, step_mm = core$step_mm, title = title, estimated = est)

stats <- ring_statistics(d, b1, step_mm = core$step_mm)
stats$scn_file <- basename(scn)
stats$core_id  <- core_id
stats <- stats[, c("scn_file", "core_id", setdiff(names(stats), c("scn_file", "core_id")))]

out_path <- file.path(out_dir, paste0(gsub("[^A-Za-z0-9_-]", "_",
            paste0(sub("\\.[^.]*$", "", basename(scn)), "_", core_id)), "_edited.csv"))
write.csv(stats, out_path, row.names = FALSE, na = "")
