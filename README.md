![R](https://img.shields.io/badge/R-4.3%2B-blue)

![License](https://img.shields.io/badge/License-MIT-green)

# Sun et al. (2026): Data and R Scripts for Meiothecium Morphometric Analyses

This repository contains the datasets and R scripts used in:

> **Sun, C.-H., Tseng, Y.-H. & Yang, J.-D. (2026).**
> *Quantitative morphometrics reveal consistent shoot-order leaf differentiation and apical-to-basal laminal cell variation in two Meiothecium (Sematophyllaceae) species in Taiwan.*
> Accepted for publication in **Hattoria**.

---

# Repository structure

```
Sun-et-al-2026-Meiothecium/
│
├── data/
│   ├── Meiothecium_leaf.xlsx
│   └── Meiothecium_cell.xlsx
│
├── scripts/
│   ├── Figure2_leaf_outlines.R
│   └── Figure3_cell_profile.R
│
├── figures/
│
├── docs/
│
├── LICENSE
└── README.md
```

---

# Requirements

R (≥ 4.3 recommended)

Required packages:

```r
install.packages(c(
  "tidyverse",
  "dplyr",
  "tidyr",
  "ggplot2",
  "readxl",
  "patchwork",
  "ragg",
  "systemfonts"
))
```

---

# Usage

Clone or download this repository.

Open R (or RStudio), set the working directory to the repository root, and run:

```r
source("scripts/Figure2_leaf_outlines.R")
source("scripts/Figure3_cell_profile.R")
```

---

# Input datasets

## Meiothecium_leaf.xlsx

Contains raw leaf morphometric measurements used to reconstruct the idealized leaf outlines (Figure 2).

Required columns:

| Column | Description |
|---------|-------------|
| species | Species name |
| voucher | Voucher specimen |
| shoot order | First- or second-order shoots |
| leaf | Leaf number |
| character | Measured character |
| value | Measured value |

The script automatically recognizes common alternative labels (e.g. *first/second*, *length of leaf*, *width of leaf*, etc.).

---

## Meiothecium_cell.xlsx

Contains laminal cell measurements used for Figure 3.

The script automatically recognizes both the original workbook column names and corrected spellings.

---

# Outputs

Running the scripts produces:

```
figures/
├── Figure2_leaf_outlines.tiff
├── Figure2_leaf_outlines.pdf
└── Figure_3_Meiothecium.tiff
```

---

# Reproducibility

All figures are generated directly from the raw measurement datasets provided in the `data/` directory.

No manual editing of numerical results is required.

---

# License

MIT License

---

# Citation

If you use these data or scripts, please cite:

Sun, C.-H., Tseng, Y.-H. & Yang, J.-D. (2026).

DOI will be added after publication.
