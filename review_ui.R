#11.06.26 02:20 NZST
# ---------------------------------------------------------------------------
# review_ui.R
# Review loop wired to the run.R output. Reads a set's output/core_summary.csv,
# takes the flagged cores worst-first, opens each in the interactive click
# editor, and writes the corrected ring table to output/edited/.
#
# Usage: Rscript review_ui.R "<set folder containing .SCN and output/>"
# Click a boundary line to remove it, a gap to add one, right-click for the next
# core. Runs on a machine with a screen.
# Requires: ring_review.R, densitometry.R
# ---------------------------------------------------------------------------

source("ring_review.R")

set_dir <- commandArgs(trailingOnly = TRUE)[1]
summary <- read.csv(file.path(set_dir, "output", "core_summary.csv"),
                    stringsAsFactors = FALSE, check.names = FALSE)
edited_dir <- file.path(set_dir, "output", "edited")
dir.create(edited_dir, recursive = TRUE, showWarnings = FALSE)

flagged <- summary[summary$review_flag %in% c(TRUE, "TRUE"), ]
flagged <- flagged[order(-flagged$review_score), ]

scn_for <- function(file) list.files(set_dir, pattern = paste0("^", file, "$"),
                       ignore.case = TRUE, full.names = TRUE, recursive = TRUE)[1]

for (i in seq_len(nrow(flagged))) {
  row   <- flagged[i, ]
  cores <- parse_scn(scn_for(row$scn_file))
  core  <- cores[[as.character(row$core_id)]]
  d     <- trim_air_channels(core$density, 200L)

  b0 <- detect_ring_boundaries(d, step_mm = core$step_mm)
  b1 <- edit_core(d, b0, step_mm = core$step_mm)

  st <- ring_statistics(d, b1, step_mm = core$step_mm)
  st$scn_file <- row$scn_file
  st$core_id  <- row$core_id
  st <- st[, c("scn_file", "core_id", setdiff(names(st), c("scn_file", "core_id")))]

  tag <- gsub("[^A-Za-z0-9_-]", "_", paste0(sub("\\.[^.]*$", "", row$scn_file), "_", row$core_id))
  write.csv(st, file.path(edited_dir, paste0(tag, "_edited.csv")), row.names = FALSE, na = "")
}
