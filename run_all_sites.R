#10.06.26 17:45 NZST
# ---------------------------------------------------------------------------
# run_all_sites.R
# Run detection and the confirmed/provisional classification over every site
# under "trials completed". Each site writes its own subfolder of per-core and
# per-ring CSV plus dual-display plots. Parity against the reference files is
# collected for every site into single combined files at the root of the run.
#
# Requires: ring_review.R, densitometry.R
# ---------------------------------------------------------------------------

source("ring_review.R")

trials_dir <- "trials completed"
out_root   <- file.path("output", "densitometry", "sites")
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

forests <- list.dirs(trials_dir, recursive = FALSE)

parity_core <- list()
parity_site <- list()
unpaired    <- list()

for (forest_dir in forests) {
  forest   <- basename(forest_dir)
  site_out <- file.path(out_root, forest)
  plot_dir <- file.path(site_out, "plots")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  scn_files <- list.files(forest_dir, pattern = "\\.scn$", ignore.case = TRUE,
                          full.names = TRUE, recursive = TRUE)
  dat_files <- list.files(forest_dir, pattern = "\\.dat$", ignore.case = TRUE,
                          full.names = TRUE, recursive = TRUE)
  scn_key <- toupper(sub("\\.scn$", "", basename(scn_files), ignore.case = TRUE))
  dat_key <- toupper(sub("\\.dat$", "", basename(dat_files), ignore.case = TRUE))
  paired  <- intersect(scn_key, dat_key)

  for (k in setdiff(scn_key, dat_key))
    unpaired[[length(unpaired) + 1L]] <- data.frame(site = forest, file = k, missing = "DAT")
  for (k in setdiff(dat_key, scn_key))
    unpaired[[length(unpaired) + 1L]] <- data.frame(site = forest, file = k, missing = "SCN")

  ring_rows  <- list()
  core_rows  <- list()
  site_diffs <- integer(0)

  for (k in paired) {
    scn <- scn_files[match(k, scn_key)]
    dat <- dat_files[match(k, dat_key)]

    results <- process_scn(scn, prominence_frac = 0.08, plot_dir = NULL)
    reference <- parse_dat(dat)

    cmp <- compare_to_dat(results, reference)
    if (nrow(cmp)) {
      cmp$site     <- forest
      cmp$scn_file <- basename(scn)
      cmp <- cmp[, c("site", "scn_file", setdiff(names(cmp), c("site", "scn_file")))]
      parity_core[[length(parity_core) + 1L]] <- cmp
      site_diffs <- c(site_diffs, cmp$diff)
    }

    for (cid in names(results)) {
      d    <- results[[cid]]$density
      b    <- results[[cid]]$boundaries
      step <- results[[cid]]$core$step_mm
      cls  <- classify_and_infill(d, b, step_mm = step)

      st <- cls$stats
      st$scn_file <- basename(scn)
      st$core_id  <- cid
      ring_rows[[length(ring_rows) + 1L]] <- st[, c("scn_file", "core_id",
        setdiff(names(st), c("scn_file", "core_id")))]

      core_rows[[length(core_rows) + 1L]] <- data.frame(
        scn_file         = basename(scn),
        core_id          = cid,
        n_confirmed      = cls$n_confirmed,
        n_provisional    = cls$n_provisional,
        n_estimated      = cls$n_estimated,
        total_estimate   = cls$n_confirmed + cls$n_provisional + cls$n_estimated,
        juvenile_zone_mm = round(cls$zone_end_ch * step, 1),
        stringsAsFactors = FALSE)

      tag <- gsub("[^A-Za-z0-9_-]", "_", paste0(sub("\\.[^.]*$", "", basename(scn)), "_", cid))
      plot_review(d, cls, step_mm = step, core_id = cid,
                  file = file.path(plot_dir, paste0(tag, ".png")))
    }
  }

  write.csv(do.call(rbind, ring_rows), file.path(site_out, "ring_detail.csv"),
            row.names = FALSE, na = "")
  write.csv(do.call(rbind, core_rows), file.path(site_out, "core_summary.csv"),
            row.names = FALSE, na = "")

  parity_site[[length(parity_site) + 1L]] <- data.frame(
    site          = forest,
    n_cores       = length(site_diffs),
    exact_pct     = round(100 * mean(site_diffs == 0), 1),
    within1_pct   = round(100 * mean(abs(site_diffs) <= 1), 1),
    mean_abs_diff = round(mean(abs(site_diffs)), 2),
    mean_bias     = round(mean(site_diffs), 2),
    stringsAsFactors = FALSE)
}

write.csv(do.call(rbind, parity_site), file.path(out_root, "parity_by_site.csv"),
          row.names = FALSE, na = "")
write.csv(do.call(rbind, parity_core), file.path(out_root, "parity_by_core.csv"),
          row.names = FALSE, na = "")
write.csv(do.call(rbind, unpaired), file.path(out_root, "unpaired_files.csv"),
          row.names = FALSE, na = "")
