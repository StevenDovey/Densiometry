#10.06.26 20:21 NZST
# ---------------------------------------------------------------------------
# review_ui.R
# Review loop across a whole run. Point it at the root; it finds every set with
# a run output (output/core_summary.csv), gathers the flagged cores from all of
# them, orders them worst-first across the whole batch, opens each in the
# interactive click editor, and writes the corrected ring table to that set's
# own output/edited/.
#
# Spacing-estimated ring proposals are shown as tomato dashed lines; click one
# to accept it as a confirmed boundary. Suspect boundaries are shown in firebrick.
#
# Click DONE to advance to the next core. Click EXIT to stop and resume later.
# Click EXIT at any time to stop — already-edited cores are saved and skipped on
# the next run. Progress is stored in review_resume.csv at the root folder.
#
# Usage: Rscript review_ui.R "<root folder>"   (defaults to the current folder)
# Requires: ring_review.R, densitometry.R
# ---------------------------------------------------------------------------

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))   # uncomment when sourcing in RStudio
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

resume_file <- file.path(root, "review_resume.csv")
if (file.exists(resume_file)) {
  rd        <- read.csv(resume_file, stringsAsFactors = FALSE)
  done_keys <- paste(rd$scn_file, rd$core_id, sep = "\t")
  W_key     <- paste(W$scn_file, W$core_id, sep = "\t")
  W         <- W[!W_key %in% done_keys, ]
}

dev.new(noRStudioGD = TRUE)
on.exit(dev.off(), add = TRUE)

for (i in seq_len(nrow(W))) {
  row  <- W[i, ]
  scn  <- list.files(row$set_dir, pattern = paste0("^", row$scn_file, "$"),
                     ignore.case = TRUE, full.names = TRUE, recursive = TRUE)[1]
  core <- parse_scn(scn)[[as.character(row$core_id)]]
  d    <- trim_air_channels(core$density, 200L)

  b0  <- detect_ring_boundaries(d, step_mm = core$step_mm)
  cls <- classify_and_infill(d, b0, step_mm = core$step_mm)

  title <- sprintf("%s  core %s   [%d of %d to check]   %s",
                   sub("\\.[^.]*$", "", row$scn_file), row$core_id, i, nrow(W),
                   basename(row$set_dir))
  b1 <- edit_core(d, b0, step_mm = core$step_mm, title = title,
                  estimated = cls$estimated)

  st <- ring_statistics(d, b1, step_mm = core$step_mm)
  st$scn_file <- row$scn_file
  st$core_id  <- row$core_id
  st <- st[, c("scn_file", "core_id", setdiff(names(st), c("scn_file", "core_id")))]

  edited_dir <- file.path(row$set_dir, "output", "edited")
  dir.create(edited_dir, recursive = TRUE, showWarnings = FALSE)
  tag <- gsub("[^A-Za-z0-9_-]", "_", paste0(sub("\\.[^.]*$", "", row$scn_file), "_", row$core_id))
  write.csv(st, file.path(edited_dir, paste0(tag, "_edited.csv")), row.names = FALSE, na = "")

  need_header <- !file.exists(resume_file)
  write.table(data.frame(scn_file = row$scn_file, core_id = row$core_id),
              resume_file, append = TRUE, sep = ",",
              col.names = need_header, row.names = FALSE, quote = TRUE)

  if (isTRUE(attr(b1, "exit"))) break
}
