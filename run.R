#11.06.26 02:00 NZST
# ---------------------------------------------------------------------------
# run.R
# Front door for new scans. Point it at a root folder; it finds every folder
# that contains .SCN files (an input set) and writes that set's results into an
# "output" folder directly under it: per-core summary with the reference-free
# review score, per-ring detail, an error-core log, and dual-display plots.
# No reference (.DAT) is needed.
#
# Usage: Rscript run.R "<root folder>"   (defaults to the current folder)
# Requires: ring_review.R, densitometry.R
# ---------------------------------------------------------------------------

source("ring_review.R")

root    <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(root)) root <- "."

scn_all <- list.files(root, pattern = "\\.scn$", ignore.case = TRUE,
                      full.names = TRUE, recursive = TRUE)
sets <- unique(dirname(scn_all))
for (s in sets) dir.create(file.path(s, "output", "plots"), recursive = TRUE, showWarnings = FALSE)

tasks <- lapply(scn_all, function(scn) list(scn = scn, out = file.path(dirname(scn), "output")))

process_one <- function(task) {
  cores <- parse_scn(task$scn)
  core_rows <- list(); ring_rows <- list(); errors <- list()
  for (cid in names(cores)) {
    d    <- trim_air_channels(cores[[cid]]$density, 200L)
    step <- cores[[cid]]$step_mm
    b    <- detect_ring_boundaries(d, step_mm = step)

    if (length(b) == 0L && max(d) < 500L) {
      errors[[length(errors) + 1L]] <- data.frame(scn_file = basename(task$scn),
        core_id = cid, n_channels = length(d), max_density = max(d), stringsAsFactors = FALSE)
      next
    }

    cls <- classify_and_infill(d, b, step_mm = step)
    sig <- review_signals(d, b, step_mm = step)

    st <- cls$stats; st$scn_file <- basename(task$scn); st$core_id <- cid
    ring_rows[[length(ring_rows) + 1L]] <- st[, c("scn_file", "core_id",
      setdiff(names(st), c("scn_file", "core_id")))]

    core_rows[[length(core_rows) + 1L]] <- data.frame(
      scn_file = basename(task$scn), core_id = cid,
      n_confirmed = cls$n_confirmed, n_provisional = cls$n_provisional,
      n_estimated = cls$n_estimated,
      total_estimate = cls$n_confirmed + cls$n_provisional + cls$n_estimated,
      juvenile_zone_mm = round(cls$zone_end_ch * step, 1),
      n_R = sig$n_R, len_mm = sig$len_mm, rhythm = sig$rhythm,
      hf = sig$hf, edge = sig$edge, n_susp = sig$n_susp, stringsAsFactors = FALSE)

    tag <- gsub("[^A-Za-z0-9_-]", "_", paste0(sub("\\.[^.]*$", "", basename(task$scn)), "_", cid))
    plot_review(d, cls, step_mm = step, core_id = cid,
                file = file.path(task$out, "plots", paste0(tag, ".png")))
  }
  list(out = task$out,
       core = do.call(rbind, core_rows),
       ring = do.call(rbind, ring_rows),
       err  = do.call(rbind, errors))
}

n_cores <- Sys.getenv("DENSI_CORES")
n_cores <- if (nzchar(n_cores)) as.integer(n_cores) else max(1L, parallel::detectCores() - 1L)
cl <- parallel::makeCluster(n_cores)
invisible(parallel::clusterCall(cl, function(wd) { setwd(wd); source("ring_review.R"); NULL }, getwd()))
out <- parallel::parLapply(cl, tasks, process_one)
parallel::stopCluster(cl)

# Group results by their output folder and write per-set files.
by_out <- split(out, vapply(out, function(o) o$out, character(1)))
for (od in names(by_out)) {
  grp  <- by_out[[od]]
  core <- do.call(rbind, lapply(grp, function(o) o$core))
  ring <- do.call(rbind, lapply(grp, function(o) o$ring))
  err  <- do.call(rbind, lapply(grp, function(o) o$err))
  core <- do.call(rbind, lapply(split(core, core$scn_file), score_review))
  write.csv(core, file.path(od, "core_summary.csv"), row.names = FALSE, na = "")
  write.csv(ring, file.path(od, "ring_detail.csv"),  row.names = FALSE, na = "")
  write.csv(err,  file.path(od, "error_cores.csv"),  row.names = FALSE, na = "")
}
