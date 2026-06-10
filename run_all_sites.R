#11.06.26 00:55 NZST
# ---------------------------------------------------------------------------
# run_all_sites.R
# Run detection and the confirmed/provisional classification over every site
# under "trials completed". Each site writes its own subfolder of per-core and
# per-ring CSV plus dual-display plots. Parity against the reference files is
# collected for every site into single combined files at the root of the run.
#
# Scan files are processed in parallel using a socket cluster, which runs on
# MS Windows, macOS and Linux. The worker count defaults to one fewer than the
# detected cores and can be set with the environment variable DENSI_CORES.
#
# Requires: ring_review.R, densitometry.R
# ---------------------------------------------------------------------------

source("ring_review.R")

trials_dir <- "trials completed"
out_root   <- file.path("output", "densitometry", "sites")
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

forests <- list.dirs(trials_dir, recursive = FALSE)

# Build the task list (one scan/reference pair per task) and the unpaired log.
tasks    <- list()
unpaired <- list()

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

  for (k in paired)
    tasks[[length(tasks) + 1L]] <- list(
      site = forest, plot_dir = plot_dir,
      scn  = scn_files[match(k, scn_key)],
      dat  = dat_files[match(k, dat_key)])
}

# Process one scan/reference pair: detect, classify, plot, and return the
# per-core parity, per-ring detail, and per-core summary rows.
process_pair <- function(task) {
  results   <- process_scn(task$scn, prominence_frac = 0.12, plot_dir = NULL)
  reference <- parse_dat(task$dat)

  cmp <- compare_to_dat(results, reference)
  if (nrow(cmp)) {
    cmp$site     <- task$site
    cmp$scn_file <- basename(task$scn)
    cmp <- cmp[, c("site", "scn_file", setdiff(names(cmp), c("site", "scn_file")))]
  }

  ring_rows <- list()
  core_rows <- list()
  for (cid in names(results)) {
    d    <- results[[cid]]$density
    b    <- results[[cid]]$boundaries
    step <- results[[cid]]$core$step_mm
    cls  <- classify_and_infill(d, b, step_mm = step)
    sig  <- review_signals(d, b, step_mm = step)

    st <- cls$stats
    st$site     <- task$site
    st$scn_file <- basename(task$scn)
    st$core_id  <- cid
    ring_rows[[length(ring_rows) + 1L]] <- st[, c("site", "scn_file", "core_id",
      setdiff(names(st), c("site", "scn_file", "core_id")))]

    core_rows[[length(core_rows) + 1L]] <- data.frame(
      site             = task$site,
      scn_file         = basename(task$scn),
      core_id          = cid,
      n_confirmed      = cls$n_confirmed,
      n_provisional    = cls$n_provisional,
      n_estimated      = cls$n_estimated,
      total_estimate   = cls$n_confirmed + cls$n_provisional + cls$n_estimated,
      juvenile_zone_mm = round(cls$zone_end_ch * step, 1),
      n_R = sig$n_R, len_mm = sig$len_mm, rhythm = sig$rhythm,
      hf = sig$hf, edge = sig$edge, n_susp = sig$n_susp,
      stringsAsFactors = FALSE)

    tag <- gsub("[^A-Za-z0-9_-]", "_", paste0(sub("\\.[^.]*$", "", basename(task$scn)), "_", cid))
    plot_review(d, cls, step_mm = step, core_id = cid,
                file = file.path(task$plot_dir, paste0(tag, ".png")))
  }

  list(parity = if (nrow(cmp)) cmp else NULL,
       ring   = do.call(rbind, ring_rows),
       core   = do.call(rbind, core_rows))
}

n_cores <- Sys.getenv("DENSI_CORES")
n_cores <- if (nzchar(n_cores)) as.integer(n_cores) else max(1L, parallel::detectCores() - 1L)

cl <- parallel::makeCluster(n_cores)
invisible(parallel::clusterCall(cl, function(wd) { setwd(wd); source("ring_review.R"); NULL }, getwd()))
out <- parallel::parLapply(cl, tasks, process_pair)
parallel::stopCluster(cl)

# Combine and write per-site and combined outputs.
all_parity <- do.call(rbind, lapply(out, function(o) o$parity))
all_ring   <- do.call(rbind, lapply(out, function(o) o$ring))
all_core   <- do.call(rbind, lapply(out, function(o) o$core))

# Review-confidence score, computed within each scan file (same-age stand).
all_core <- do.call(rbind, lapply(split(all_core, all_core$scn_file), score_review))

for (forest in unique(all_core$site)) {
  site_out <- file.path(out_root, forest)
  write.csv(all_ring[all_ring$site == forest, ], file.path(site_out, "ring_detail.csv"),
            row.names = FALSE, na = "")
  write.csv(all_core[all_core$site == forest, ], file.path(site_out, "core_summary.csv"),
            row.names = FALSE, na = "")
}

parity_site <- do.call(rbind, lapply(unique(all_parity$site), function(s) {
  d <- all_parity$diff[all_parity$site == s]
  data.frame(site = s, n_cores = length(d),
             exact_pct     = round(100 * mean(d == 0), 1),
             within1_pct   = round(100 * mean(abs(d) <= 1), 1),
             mean_abs_diff = round(mean(abs(d)), 2),
             mean_bias     = round(mean(d), 2),
             stringsAsFactors = FALSE)
}))

write.csv(parity_site, file.path(out_root, "parity_by_site.csv"), row.names = FALSE, na = "")
write.csv(all_parity,  file.path(out_root, "parity_by_core.csv"), row.names = FALSE, na = "")
write.csv(do.call(rbind, unpaired), file.path(out_root, "unpaired_files.csv"),
          row.names = FALSE, na = "")
