#10.06.26 20:21 NZST
# ---------------------------------------------------------------------------
# review_run.R
# New-scan driver. Run ring detection and the confirmed/provisional
# classification over a folder of .SCN files, with no reference needed, render
# the dual-display plots, and write per-core and per-ring results, an
# error-core log, and a reference-free review score so off cores sort first.
#
# Usage: Rscript review_run.R "<folder containing .SCN files>"
# Requires: ring_review.R, densitometry.R
# ---------------------------------------------------------------------------

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))   # uncomment when sourcing in RStudio
source("ring_review.R")

base_dir   <- commandArgs(trailingOnly = TRUE)[1]
out_dir    <- file.path("output", "densitometry", "review", basename(base_dir))
plot_dir   <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

scn_files  <- list.files(base_dir, pattern = "\\.scn$", ignore.case = TRUE,
                         full.names = TRUE, recursive = TRUE)

per_core   <- list()
per_ring   <- list()
errors     <- list()

for (scn in scn_files) {
  cores <- parse_scn(scn)

  for (cid in names(cores)) {
    d <- trim_air_channels(cores[[cid]]$density, 200L)
    b <- detect_ring_boundaries(d, step_mm = cores[[cid]]$step_mm, prominence_frac = 0.12)

    is_error <- length(b) == 0L && max(d) < 500L
    if (is_error) {
      errors[[length(errors) + 1L]] <- data.frame(
        scn_file = basename(scn), core_id = cid,
        n_channels = length(d), max_density = max(d),
        stringsAsFactors = FALSE)
      next
    }

    cls <- classify_and_infill(d, b, step_mm = cores[[cid]]$step_mm)
    sig <- review_signals(d, b, step_mm = cores[[cid]]$step_mm)
    plot_review(d, cls, step_mm = cores[[cid]]$step_mm, core_id = cid,
                file = file.path(plot_dir, paste0(gsub("[^A-Za-z0-9_-]", "_",
                       paste0(sub("\\.[^.]*$", "", basename(scn)), "_", cid)), ".png")))

    st <- cls$stats
    st$scn_file <- basename(scn)
    st$core_id  <- cid
    per_ring[[length(per_ring) + 1L]] <- st[, c("scn_file", "core_id",
      setdiff(names(st), c("scn_file", "core_id")))]

    per_core[[length(per_core) + 1L]] <- data.frame(
      scn_file        = basename(scn),
      core_id         = cid,
      n_confirmed     = cls$n_confirmed,
      n_provisional   = cls$n_provisional,
      n_estimated     = cls$n_estimated,
      total_estimate  = cls$n_confirmed + cls$n_provisional + cls$n_estimated,
      juvenile_zone_mm = round(cls$zone_end_ch * cores[[cid]]$step_mm, 1),
      n_R = sig$n_R, len_mm = sig$len_mm, rhythm = sig$rhythm,
      hf = sig$hf, edge = sig$edge, n_susp = sig$n_susp,
      stringsAsFactors = FALSE)
  }
}

core_tbl <- do.call(rbind, per_core)
core_tbl <- do.call(rbind, lapply(split(core_tbl, core_tbl$scn_file), score_review))
write.csv(core_tbl, file.path(out_dir, "core_summary.csv"),
          row.names = FALSE, na = "")
write.csv(do.call(rbind, per_ring), file.path(out_dir, "ring_detail.csv"),
          row.names = FALSE, na = "")
write.csv(do.call(rbind, errors), file.path(out_dir, "error_cores.csv"),
          row.names = FALSE, na = "")
