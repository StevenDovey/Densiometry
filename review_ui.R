#11.06.26 02:45 NZST
# ---------------------------------------------------------------------------
# review_ui.R
# Review loop across a whole run. Point it at the root; it finds every set with
# a run output (output/core_summary.csv), gathers the flagged cores from all of
# them, orders them worst-first across the whole batch, opens each in the
# interactive click editor, and writes the corrected ring table to that set's
# own output/edited/.
#
# Usage: Rscript review_ui.R "<root folder>"   (defaults to the current folder)
# Click a boundary line to remove it, a gap to add one, right-click for the next
# core. Runs on a machine with a screen.
# Requires: ring_review.R, densitometry.R
# ---------------------------------------------------------------------------

source("ring_review.R")

root <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(root)) root <- "."

summaries <- list.files(root, pattern = "^core_summary\\.csv$",
                        full.names = TRUE, recursive = TRUE)
summaries <- summaries[basename(dirname(summaries)) == "output"]

work <- list()
for (sf in summaries) {
  set_dir <- dirname(dirname(sf))
  s <- read.csv(sf, stringsAsFactors = FALSE, check.names = FALSE)
  s <- s[s$review_flag %in% c(TRUE, "TRUE"), ]
  if (nrow(s)) { s$set_dir <- set_dir; work[[length(work) + 1L]] <- s }
}
W <- do.call(rbind, work)
W <- W[order(-W$review_score), ]

for (i in seq_len(nrow(W))) {
  row     <- W[i, ]
  scn     <- list.files(row$set_dir, pattern = paste0("^", row$scn_file, "$"),
                        ignore.case = TRUE, full.names = TRUE, recursive = TRUE)[1]
  core    <- parse_scn(scn)[[as.character(row$core_id)]]
  d       <- trim_air_channels(core$density, 200L)

  b1 <- edit_core(d, detect_ring_boundaries(d, step_mm = core$step_mm), step_mm = core$step_mm)

  st <- ring_statistics(d, b1, step_mm = core$step_mm)
  st$scn_file <- row$scn_file
  st$core_id  <- row$core_id
  st <- st[, c("scn_file", "core_id", setdiff(names(st), c("scn_file", "core_id")))]

  edited_dir <- file.path(row$set_dir, "output", "edited")
  dir.create(edited_dir, recursive = TRUE, showWarnings = FALSE)
  tag <- gsub("[^A-Za-z0-9_-]", "_", paste0(sub("\\.[^.]*$", "", row$scn_file), "_", row$core_id))
  write.csv(st, file.path(edited_dir, paste0(tag, "_edited.csv")), row.names = FALSE, na = "")
}
