## ============================================================
## Figure 2: Meiothecium idealized leaf outlines
##
## Sun et al. (2026)
## Quantitative morphometrics reveal consistent shoot-order
## leaf differentiation and apical-to-basal laminal cell
## variation in two Meiothecium (Sematophyllaceae) species
## in Taiwan
##
## Accepted for publication in Hattoria
##
## Description
## Idealized leaf outlines reconstructed from:
##
## - leaf length (LL)
## - maximum leaf width (LW)
## - position of maximum leaf width (Wp)
##
## Line types:
##
## - Mean: black solid line
## - Maximum: black short-dashed line
## - Minimum: grey solid line
##
## The minimum and maximum outlines combine the respective
## minimum or maximum values of LL, LW, and Wp. They therefore
## represent idealized extreme outlines and do not necessarily
## correspond to individual observed leaves.
##
## Outputs:
##
## - 600 dpi TIFF with LZW compression
## - Vector PDF
## ============================================================

## ------------------------------------------------------------
## 0. Load packages
## ------------------------------------------------------------

required_packages <- c(
  "readxl",
  "tidyr",
  "dplyr",
  "ggplot2"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "The following R packages are required but not installed: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them with:\ninstall.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    )
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

## ------------------------------------------------------------
## 1. Define input and output paths
## ------------------------------------------------------------

data_file <- file.path(
  "data",
  "Meiothecium_leaf.xlsx"
)

tiff_file <- file.path(
  "figures",
  "Figure2_leaf_outlines.tiff"
)

pdf_file <- file.path(
  "figures",
  "Figure2_leaf_outlines.pdf"
)

if (!file.exists(data_file)) {
  stop(
    paste0(
      "Input file not found: ",
      data_file,
      "\n\nRun this script from the repository root directory."
    )
  )
}

if (!dir.exists("figures")) {
  dir.create(
    "figures",
    recursive = TRUE
  )
}

## ------------------------------------------------------------
## 2. Read raw morphometric data
## ------------------------------------------------------------

dat <- readxl::read_excel(
  path = data_file
)

names(dat) <- trimws(names(dat))

## ------------------------------------------------------------
## 3. Check required columns
## ------------------------------------------------------------

required_columns <- c(
  "species",
  "voucher",
  "shoot_order",
  "leaf",
  "character",
  "value"
)

missing_columns <- setdiff(
  required_columns,
  names(dat)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The input file is missing the following columns: ",
      paste(missing_columns, collapse = ", ")
    )
  )
}

## ------------------------------------------------------------
## 4. Clean and standardize raw data
## ------------------------------------------------------------

dat <- dat %>%
  mutate(
    species = trimws(as.character(species)),
    voucher = trimws(as.character(voucher)),
    shoot_order = tolower(trimws(as.character(shoot_order))),
    leaf = trimws(as.character(leaf)),
    character = trimws(as.character(character)),
    value = suppressWarnings(as.numeric(value))
  )

dat$character <- gsub(
  pattern = "L/W",
  replacement = "L_W",
  x = dat$character,
  fixed = TRUE
)

dat <- dat %>%
  mutate(
    species = factor(
      species,
      levels = c(
        "M. intextum",
        "M. microcarpum"
      )
    ),
    shoot_order = factor(
      shoot_order,
      levels = c(
        "primary",
        "secondary"
      )
    )
  ) %>%
  filter(
    !is.na(species),
    !is.na(shoot_order),
    !is.na(character),
    !is.na(value)
  )

if (nrow(dat) == 0) {
  stop(
    "No usable observations remained after data cleaning. ",
    "Check species names, shoot_order values, character names, and numeric values."
  )
}

## ------------------------------------------------------------
## 5. Calculate summary statistics from raw observations
## ------------------------------------------------------------

summary_dat <- dat %>%
  group_by(
    species,
    shoot_order,
    character
  ) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

wide_mean <- pivot_wider(
  data = summary_dat,
  id_cols = c(species, shoot_order),
  names_from = character,
  values_from = mean
)

wide_min <- pivot_wider(
  data = summary_dat,
  id_cols = c(species, shoot_order),
  names_from = character,
  values_from = min
)

wide_max <- pivot_wider(
  data = summary_dat,
  id_cols = c(species, shoot_order),
  names_from = character,
  values_from = max
)

need_cols <- c(
  "LL",
  "LW",
  "Wp"
)

check_required_traits <- function(data, object_name) {
  missing_traits <- setdiff(
    need_cols,
    names(data)
  )

  if (length(missing_traits) > 0) {
    stop(
      paste0(
        object_name,
        " is missing the following traits: ",
        paste(missing_traits, collapse = ", "),
        "\nCheck the values in the 'character' column of ",
        data_file,
        "."
      )
    )
  }
}

check_required_traits(wide_mean, "wide_mean")
check_required_traits(wide_min, "wide_min")
check_required_traits(wide_max, "wide_max")

wide_mean <- wide_mean[
  ,
  c("species", "shoot_order", need_cols)
]

wide_min <- wide_min[
  ,
  c("species", "shoot_order", need_cols)
]

wide_max <- wide_max[
  ,
  c("species", "shoot_order", need_cols)
]

wide_mean <- wide_mean[complete.cases(wide_mean), ]
wide_min <- wide_min[complete.cases(wide_min), ]
wide_max <- wide_max[complete.cases(wide_max), ]

if (
  nrow(wide_mean) == 0 ||
  nrow(wide_min) == 0 ||
  nrow(wide_max) == 0
) {
  stop(
    "No complete species × shoot-order combinations were available for LL, LW, and Wp."
  )
}

## ------------------------------------------------------------
## 6. Idealized leaf outline model
## ------------------------------------------------------------

leaf_curves_moss <- function(
    LL,
    LW,
    Wp,
    n = 420,
    base_power = 0.65,
    apex_power = 1.85
) {
  if (
    length(LL) != 1 ||
    length(LW) != 1 ||
    length(Wp) != 1 ||
    !is.finite(LL) ||
    !is.finite(LW) ||
    !is.finite(Wp) ||
    LL <= 0 ||
    LW <= 0
  ) {
    stop(
      "LL and LW must be positive finite values, and Wp must be a finite value."
    )
  }

  x <- seq(
    from = 0,
    to = LL,
    length.out = n
  )

  width <- numeric(length(x))

  Wp <- max(
    min(Wp, LL * 0.95),
    LL * 0.05
  )

  for (i in seq_along(x)) {
    if (x[i] <= Wp) {
      width[i] <- LW * (x[i] / Wp)^base_power
    } else {
      width[i] <- LW *
        (
          1 -
            (
              (x[i] - Wp) /
                (LL - Wp)
            )^apex_power
        )
    }
  }

  width[width < 0] <- 0

  data.frame(
    x = x,
    upper = width / 2,
    lower = -width / 2
  )
}

## ------------------------------------------------------------
## 7. Build curves
## ------------------------------------------------------------

build_curves <- function(wide, tag) {
  do.call(
    rbind,
    lapply(
      seq_len(nrow(wide)),
      function(i) {
        row_data <- wide[i, ]

        curve_data <- leaf_curves_moss(
          LL = row_data$LL,
          LW = row_data$LW,
          Wp = row_data$Wp
        )

        curve_data$species <- row_data$species
        curve_data$shoot_order <- row_data$shoot_order
        curve_data$type <- tag

        curve_data
      }
    )
  )
}

cur_mean <- build_curves(wide_mean, "mean")
cur_min <- build_curves(wide_min, "min")
cur_max <- build_curves(wide_max, "max")

## ------------------------------------------------------------
## 8. Construct closed outline paths
## ------------------------------------------------------------

make_closed_path <- function(cur) {
  groups <- split(
    cur,
    interaction(
      cur$species,
      cur$shoot_order,
      drop = TRUE
    )
  )

  closed_paths <- lapply(
    groups,
    function(df) {
      data.frame(
        x = c(df$x, rev(df$x), df$x[1]),
        y = c(df$upper, rev(df$lower), df$upper[1]),
        species = df$species[1],
        shoot_order = df$shoot_order[1]
      )
    }
  )

  do.call(rbind, closed_paths)
}

mean_path <- make_closed_path(cur_mean)
min_path <- make_closed_path(cur_min)
max_path <- make_closed_path(cur_max)

species_levels <- c(
  "M. intextum",
  "M. microcarpum"
)

shoot_order_levels <- c(
  "primary",
  "secondary"
)

restore_factors <- function(data) {
  data %>%
    mutate(
      species = factor(
        species,
        levels = species_levels
      ),
      shoot_order = factor(
        shoot_order,
        levels = shoot_order_levels
      )
    )
}

mean_path <- restore_factors(mean_path)
min_path <- restore_factors(min_path)
max_path <- restore_factors(max_path)

## ------------------------------------------------------------
## 9. Define one shared scale bar
## ------------------------------------------------------------

x_min_all <- min(max_path$x, na.rm = TRUE)
y_min_all <- min(max_path$y, na.rm = TRUE)

bar_len <- 0.5
bar_x0 <- x_min_all + 0.10
bar_x1 <- bar_x0 + bar_len
bar_y <- y_min_all + 0.10

scale_bar_df <- data.frame(
  species = factor(
    "M. intextum",
    levels = species_levels
  ),
  shoot_order = factor(
    "secondary",
    levels = shoot_order_levels
  ),
  x = bar_x0,
  xend = bar_x1,
  y = bar_y,
  yend = bar_y
)

scale_label_df <- data.frame(
  species = factor(
    "M. intextum",
    levels = species_levels
  ),
  shoot_order = factor(
    "secondary",
    levels = shoot_order_levels
  ),
  x = (bar_x0 + bar_x1) / 2,
  y = bar_y - 0.06,
  label = "0.5 mm"
)

## ------------------------------------------------------------
## 10. Line settings
## ------------------------------------------------------------

lw_mean <- 1.4
lw_max <- 1.0
lw_min <- 0.9

col_mean <- "black"
col_max <- "black"
col_min <- "grey55"

lt_mean <- "solid"
lt_max <- "22"
lt_min <- "solid"

## ------------------------------------------------------------
## 11. Create plot
## ------------------------------------------------------------

p <- ggplot() +
  geom_path(
    data = max_path,
    aes(
      x = x,
      y = y,
      group = interaction(species, shoot_order)
    ),
    color = col_max,
    linewidth = lw_max,
    linetype = lt_max,
    lineend = "round",
    linejoin = "round"
  ) +
  geom_path(
    data = min_path,
    aes(
      x = x,
      y = y,
      group = interaction(species, shoot_order)
    ),
    color = col_min,
    linewidth = lw_min,
    linetype = lt_min,
    lineend = "round",
    linejoin = "round"
  ) +
  geom_path(
    data = mean_path,
    aes(
      x = x,
      y = y,
      group = interaction(species, shoot_order)
    ),
    color = col_mean,
    linewidth = lw_mean,
    linetype = lt_mean,
    lineend = "round",
    linejoin = "round"
  ) +
  facet_grid(
    shoot_order ~ species
  ) +
  coord_equal() +
  theme_void() +
  theme(
    strip.text = element_text(
      size = 12,
      face = "italic",
      family = "Arial"
    ),
    text = element_text(
      family = "Arial"
    ),
    panel.spacing = unit(
      1.2,
      "lines"
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  ) +
  geom_segment(
    data = scale_bar_df,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.9,
    lineend = "butt"
  ) +
  geom_text(
    data = scale_label_df,
    aes(
      x = x,
      y = y,
      label = label
    ),
    inherit.aes = FALSE,
    color = "black",
    size = 3,
    family = "Arial"
  )

print(p)

## ------------------------------------------------------------
## 12. Save outputs
## ------------------------------------------------------------

ggsave(
  filename = tiff_file,
  plot = p,
  width = 180,
  height = 110,
  units = "mm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = pdf_file,
  plot = p,
  width = 180,
  height = 110,
  units = "mm",
  bg = "white"
)

cat(
  "\nFigure export completed.\n",
  "\nTIFF: ",
  tiff_file,
  "\nPDF: ",
  pdf_file,
  "\n",
  sep = ""
)
