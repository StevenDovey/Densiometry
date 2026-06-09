# Densiometry

R reimplementation of the **EDITOR** data-processing pipeline for the
FRI Direct-Scanning X-Ray Densitometer (Cown & Clement 1983; system software
upgraded in the 1990s), maintained at BSI (formerly Forest Research Institute),
Rotorua, NZ.

---

## Background

The FRI densitometer uses a Fe-55 radioisotopic source and a scintillation
detector to measure wood density at 0.3 mm spatial resolution along machined
increment cores (2 mm thick, 5 mm increment).  Raw count data are written to
`.SCN` files by the SCANNA acquisition program.

The original **EDITOR** program reads `.SCN` files, lets the operator check and
correct annual ring boundaries, and outputs per-ring statistics (ring width,
earlywood/latewood widths and densities, etc.).  This repository reimplements
that pipeline in R with no dependencies beyond base R and the `stats` package.

**Reference:** Cown, D.J. & Clement, B.C. (1983). A wood densitometer using
direct scanning with X-rays. *Wood Science and Technology* **17**: 91–99.

---

## What is (and isn't) reproducible from a `.SCN`

EDITOR is an **interactive** program: an operator corrects ring boundaries and
**assigns calendar years** to rings.  Those decisions are *not* stored in the
`.SCN` file, so no automatic program can reproduce the finished `.DAT` exactly
from the `.SCN` alone.  This reimplementation therefore:

- **labels rings by position** — `ring_no` 1, 2, 3 … from the innermost
  captured ring outward (no year required);
- **detects ring boundaries automatically** as a first pass, and **flags
  suspect / possible false (intra-annual) rings** for operator review;
- leaves the **calendar year as a column the operator adds downstream**
  (`year = pith_year + ring_no − 1`).

Given identical boundaries, the per-ring widths, areas and densities match the
DOS EDITOR's arithmetic — so parity holds for the *computation*; the only
human-in-the-loop part is confirming the boundaries.

---

## Repository contents

| File | Description |
|------|-------------|
| `densitometry.R` | Core library — `parse_scn()`, `trim_air_channels()`, `detect_ring_boundaries()`, `ring_statistics()`, `format_editor_output()`, `plot_density_profile()`, `process_scn()` |
| `process_scn.R` | Driver script — runs the full pipeline on `AK6.SCN`, writes per-core/combined CSVs and annotated PNG plots to `output/densitometry/` |
| `parity_check.R` | Compares the detector against `AK6.DAT` and searches the best per-core `prominence_frac`; writes parity/calibration CSVs |
| `AK6.SCN` | Example raw scan: Akarana Road, Wharerata Forest (Red Needle Cast study) |
| `AK6.DAT` | Operator-edited reference output for the same cores (parity check) |

Operator/analysis helpers in `densitometry.R`: `parse_dat()`, `compare_to_dat()`,
`calibrate_prominence()`, `calibrate_to_dat()`, `apply_ring_edits()`,
`review_suspects()`.

---

## Pipeline

```
parse_scn()            # Read .SCN → named list of core objects
  └─ trim_air_channels()        # Remove leading air-artefact channels
       └─ detect_ring_boundaries()   # Prominence-based ring detection + suspect flags
            └─ ring_statistics()     # Per-ring mm widths, areas, EW/LW densities
                 └─ format_editor_output()  # Console table (ring-number labelled)
                      └─ plot_density_profile()  # Annotated density-trace PNG
```

### Ring detection

A single fixed density cut-off (e.g. 500 kg/m³) misses the weak latewood of
juvenile inner rings.  Instead, boundaries are found by **latewood-peak
prominence**:

1. smooth the trace (5-point running mean);
2. find latewood peaks (local maxima) and earlywood troughs (local minima);
3. keep peaks whose prominence exceeds an **adaptive** threshold
   (`prominence_frac × (max − min)`), so weak inner-ring latewood is retained;
4. enforce a minimum ring width;
5. place each boundary at the steepest density drop after a latewood peak.

Rings are flagged **suspect** (possible false / intra-annual, weak latewood,
low prominence, or anomalously narrow) and surfaced for operator review rather
than silently split or merged.

### Parity vs the operator-edited `.DAT` (AK6, prominence_frac = 0.08)

| Metric | Result |
|--------|--------|
| Cores with exact ring count | 4 / 14 |
| Mean absolute error | **≈ 1.4 rings/core** (down from ≈ 6 with a fixed 500 cut-off) |
| Total rings | R 354 vs DAT 349 |

The residual ±1–3 differences are exactly the false-ring/merge calls the
operator resolves by hand — supported here by the `suspect` flag and
`manual_boundaries` override.

---

## Quick start

```r
source("densitometry.R")

results <- process_scn(
  filepath        = "AK6.SCN",
  ew_lw_threshold = 500L,    # kg/m³ — EW/LW boundary for radiata pine
  min_ring_mm     = 2,
  smooth_n        = 5L,
  air_threshold   = 200L,
  prominence_frac = 0.08,    # adaptive latewood-peak prominence cut-off
  plot_dir        = "output/densitometry/plots"
)
```

Per-ring output columns (all widths/radii in **mm**, areas in **cm²**):
`ring_no`, `inner_radius_mm`, `outer_radius_mm`, `ring_width_mm`,
`ew_width_mm`, `lw_width_mm`, `pct_latewood`, `incr_area_cm2`,
`total_area_cm2`, `ring_mean`, `ew_density`, `lw_density`, `min_density`,
`max_density`, `uniformity`, `range_density`, `lw_peak_density`,
`prominence`, `partial_ring`, `ew_only`, `suspect`, `suspect_reason`.

### Manual ring boundary override & pith offset

After reviewing a core's annotated plot, correct it by channel position and/or
set its pith ring offset:

```r
results <- process_scn(
  filepath          = "AK6.SCN",
  manual_boundaries = list("3" = c(45L, 130L, 210L)),
  rings_offset      = list("1" = 2L)   # first captured ring is ring 3
)
```

### Reviewing suspect rings

Step through a core's flagged rings and accept / merge / split each.  In an
interactive session `review_suspects()` prompts per ring; passing `decisions`
runs it unattended (and makes the workflow scriptable):

```r
d <- trim_air_channels(parse_scn("AK6.SCN")[["1"]]$density)
b <- detect_ring_boundaries(d, prominence_frac = 0.08)

# unattended: dissolve the false ring at ring 8, split ring 25 at channel 690
fixed <- review_suspects(d, b,
  decisions = list("8" = "merge-left", "25" = "split 690"))
```

### Per-core tuning against a `.DAT`

`prominence_frac` is the one knob that trades sensitivity (catching weak inner
rings) against false positives.  When an edited `.DAT` exists, calibrate it per
core automatically:

```r
cores <- parse_scn("AK6.SCN");  dat <- parse_dat("AK6.DAT")
calibrate_to_dat(cores, dat)    # best prominence_frac + ring count per core
```

On AK6 this raises exact ring-count agreement from 4/15 to **10/15 cores**.
Run `Rscript parity_check.R` for the full report.

---

## .SCN file format

Each core block in a `.SCN` file consists of:

```
#### <core_id> <field2> <critical_count> <attenuation_factor> <step_mm> <air_count> <scan_code> [aux]
<density values, integers in kg/m³, multiple per line>
****
```

- `step_mm` — spatial resolution (0.3 mm after the 1990s upgrade)
- `field2`, `scan_code` — operator/acquisition metadata; **not** the pith
  offset or calendar year (those are assigned during editing and are absent
  from the `.SCN`)
- Two-piece scans (suffix `a`/`b`) are automatically concatenated by `parse_scn()`

---

## Species and studies

Developed primarily for **radiata pine** (*Pinus radiata* D.Don) but applies to
any species scanned by the FRI densitometer.  Archived here:

- Red Needle Cast effects on growth and wood density (Wharerata Forest — AK6)

---

## Requirements

- R ≥ 4.0
- Base R only (`stats` + `grDevices`, included with every R installation)
- No tidyverse or other external packages required
