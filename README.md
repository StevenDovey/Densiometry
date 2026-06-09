# Densiometry

R reimplementation of the **EDITOR** data-processing pipeline for the
FRI Direct-Scanning X-Ray Densitometer (Cown & Clement 1983),
maintained at BSI (formerly Forest Research Institute), Rotorua, NZ.

---

## Background

The FRI densitometer uses a Fe-55 radioisotopic source and a scintillation
detector to measure wood density at 0.3 mm spatial resolution along machined
increment cores (2 mm thick, 5 mm increment).  Raw count data are written to
`.SCN` files by the SCANNA acquisition program.

The original **EDITOR** program (Cown & Clement 1983) reads `.SCN` files,
detects annual ring boundaries from the density trace, and outputs per-ring
statistics (ring width, earlywood/latewood widths and densities, etc.).
This repository reimplements that pipeline in R with no external dependencies
beyond base R and the `stats` package.

**Reference:** Cown, D.J. & Clement, B.C. (1983). A wood densitometer using
direct scanning with X-rays. *Wood Science and Technology* **17**: 91–99.

---

## Repository contents

| File | Description |
|------|-------------|
| `densitometry.R` | Core library — `parse_scn()`, `trim_air_channels()`, `detect_ring_boundaries()`, `ring_statistics()`, `format_editor_output()`, `plot_density_profile()`, `process_scn()` |
| `process_scn.R` | Driver script — runs the full pipeline on `AK6.SCN` and writes per-core and combined CSV output to `output/densitometry/` |
| `AK6.SCN` | Example raw scan file: Akarana Road, Wharerata Forest (Red Needle Cast study, 14 cores, scanned 1993) |

---

## Pipeline

```
parse_scn()            # Read .SCN → named list of core objects
  └─ trim_air_channels()        # Remove leading air-artefact channels
       └─ detect_ring_boundaries()   # Threshold-crossing ring detection
            └─ ring_statistics()     # Per-ring EW/LW summary statistics
                 └─ format_editor_output()  # Console table (EDITOR format)
                      └─ plot_density_profile()  # Base R density trace plot
```

`process_scn()` is the top-level wrapper that runs all steps for every core
in a `.SCN` file.

---

## Quick start

```r
source("densitometry.R")

results <- process_scn(
  filepath        = "AK6.SCN",
  ew_lw_threshold = 500L,   # kg/m³ — standard for radiata pine
  min_ring_mm     = 2,
  smooth_n        = 5L,
  air_threshold   = 200L,
  plot            = TRUE
)
```

Output columns match the original EDITOR program:
`ring_from_pith`, `year`, `outer_radius_mm`, `ring_width_mm`,
`ew_width_mm`, `lw_width_mm`, `pct_latewood`, `ring_mean`,
`ew_density`, `lw_density`, `min_density`, `max_density`,
`uniformity`, `range_density`, `partial_ring`, `ew_only`.

### Manual ring boundary override

If automatic detection misses or splits a ring for a specific core:

```r
results <- process_scn(
  filepath          = "AK6.SCN",
  manual_boundaries = list("3" = c(45L, 130L, 210L))
)
```

---

## .SCN file format

Each core block in a `.SCN` file consists of:

```
#### <core_id> <rings_offset> <critical_count> <attenuation_factor> <step_mm> <air_count> <scan_year> [unknown]
<density values, integers in kg/m³, 20 per line>
****
```

- `rings_offset` — rings from pith not captured before this scan (0 = starts at pith)
- `step_mm` — spatial resolution (0.3 mm after the 1990s upgrade)
- `scan_year` — calendar year of the outermost (most recent) ring
- Two-piece scans (suffix `a`/`b`) are automatically concatenated by `parse_scn()`

---

## Species and studies

The pipeline was developed primarily for **radiata pine** (*Pinus radiata* D.Don)
but applies to any species scanned by the FRI densitometer.  Studies archived
here include investigations of:

- Red Needle Cast effects on growth and wood density (Wharerata Forest — AK6)

---

## Requirements

- R ≥ 4.0
- Base R only (`stats` package, included with every R installation)
- No tidyverse or other external packages required
