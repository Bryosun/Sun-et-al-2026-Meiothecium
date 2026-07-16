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

suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(grid)
})


## ------------------------------------------------------------
## 1. Define input and output paths
## ------------------------------------------------------------
##
## Run this script from the repository root directory:
##
## source("scripts/Figure2_leaf_outlines.R")
## ------------------------------------------------------------

data_file <- file.path(
  "data",
  "Meiothecium_leaf_summary.csv"
)

tiff_file <- file.path(
  "figures",
  "Figure2_leaf_outlines.tiff"
)

pdf_file <- file.path(
  "figures",
  "Figure2_leaf_outlines.pdf"
)


## Check that the input file exists

if (!file.exists(data_file)) {
  stop(
    paste0(
      "Input file not found: ",
      data_file,
      "\n\nRun this script from the repository root directory."
    )
  )
}


## Create the output directory if necessary

if (!dir.exists("figures")) {
  dir.create(
    "figures",
    recursive = TRUE
  )
}


## ------------------------------------------------------------
## 2. Read morphometric summary data
## ------------------------------------------------------------
##
## Required CSV columns:
##
## species
## shoot_order
## character
## mean
## sd
## min
## max
## ------------------------------------------------------------

dat <- read.csv(
  file = data_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


## ------------------------------------------------------------
## 3. Check required columns
## ------------------------------------------------------------

required_columns <- c(
  "species",
  "shoot_order",
  "character",
  "mean",
  "sd",
  "min",
  "max"
)

missing_columns <- setdiff(
  required_columns,
  names(dat)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The input file is missing the following columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


## ------------------------------------------------------------
## 4. Clean and standardize data
## ------------------------------------------------------------

dat <- dat %>%
  mutate(
    species = trimws(
      as.character(species)
    ),

    shoot_order = tolower(
      trimws(
        as.character(shoot_order)
      )
    ),

    character = trimws(
      as.character(character)
    ),

    mean = suppressWarnings(
      as.numeric(mean)
    ),

    sd = suppressWarnings(
      as.numeric(sd)
    ),

    min = suppressWarnings(
      as.numeric(min)
    ),

    max = suppressWarnings(
      as.numeric(max)
    )
  )


## Standardize character names

dat$character <- gsub(
  pattern = "L/W",
  replacement = "L_W",
  x = dat$character,
  fixed = TRUE
)


## Define display order

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
    !is.na(character)
  )


## ------------------------------------------------------------
## 5. Convert summary data to wide format
## ------------------------------------------------------------

wide_mean <- pivot_wider(
  data = dat,
  id_cols = c(
    species,
    shoot_order
  ),
  names_from = character,
  values_from = mean
)

wide_min <- pivot_wider(
  data = dat,
  id_cols = c(
    species,
    shoot_order
  ),
  names_from = character,
  values_from = min
)

wide_max <- pivot_wider(
  data = dat,
  id_cols = c(
    species,
    shoot_order
  ),
  names_from = character,
  values_from = max
)


## Traits required for outline reconstruction

need_cols <- c(
  "LL",
  "LW",
  "Wp"
)


## Check that all required traits are present

check_required_traits <- function(
    data,
    object_name
) {
  missing_traits <- setdiff(
    need_cols,
    names(data)
  )

  if (length(missing_traits) > 0) {
    stop(
      paste0(
        object_name,
        " is missing the following traits: ",
        paste(
          missing_traits,
          collapse = ", "
        )
      )
    )
  }
}


check_required_traits(
  wide_mean,
  "wide_mean"
)

check_required_traits(
  wide_min,
  "wide_min"
)

check_required_traits(
  wide_max,
  "wide_max"
)


## Keep only the traits used in the reconstruction

wide_mean <- wide_mean[
  ,
  c(
    "species",
    "shoot_order",
    need_cols
  )
]

wide_min <- wide_min[
  ,
  c(
    "species",
    "shoot_order",
    need_cols
  )
]

wide_max <- wide_max[
  ,
  c(
    "species",
    "shoot_order",
    need_cols
  )
]


## Remove incomplete combinations

wide_mean <- wide_mean[
  complete.cases(wide_mean),
]

wide_min <- wide_min[
  complete.cases(wide_min),
]

wide_max <- wide_max[
  complete.cases(wide_max),
]


## ------------------------------------------------------------
## 6. Idealized leaf outline model
## ------------------------------------------------------------
##
## The outline is reconstructed using two power functions:
##
## 1. Leaf base to the position of maximum width
## 2. Position of maximum width to the leaf apex
##
## The resulting half-width profile is mirrored around the
## longitudinal axis to create a symmetrical closed outline.
## ------------------------------------------------------------

leaf_curves_moss <- function(
    LL,
    LW,
    Wp,
    n = 420,
    base_power = 0.65,
    apex_power = 1.85
) {

  ## Validate inputs

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
      paste0(
        "LL and LW must be positive finite values, ",
        "and Wp must be a finite value."
      )
    )
  }


  ## Longitudinal positions along the leaf

  x <- seq(
    from = 0,
    to = LL,
    length.out = n
  )


  ## Initialize the width profile

  width <- numeric(
    length(x)
  )


  ## Prevent Wp from falling exactly at the base or apex

  Wp <- max(
    min(
      Wp,
      LL * 0.95
    ),
    LL * 0.05
  )


  ## Calculate width at each longitudinal position

  for (i in seq_along(x)) {

    if (x[i] <= Wp) {

      ## Leaf base to maximum width

      width[i] <- LW *
        (
          x[i] / Wp
        )^base_power

    } else {

      ## Maximum width to leaf apex

      width[i] <- LW *
        (
          1 -
            (
              (
                x[i] - Wp
              ) /
                (
                  LL - Wp
                )
            )^apex_power
        )
    }
  }


  ## Remove possible negative values caused by rounding

  width[
    width < 0
  ] <- 0


  ## Return upper and lower half-width coordinates

  data.frame(
    x = x,
    upper = width / 2,
    lower = -width / 2
  )
}


## ------------------------------------------------------------
## 7. Build curves for all species and shoot orders
## ------------------------------------------------------------

build_curves <- function(
    wide,
    tag
) {

  do.call(
    rbind,
    lapply(
      seq_len(
        nrow(wide)
      ),
      function(i) {

        row_data <- wide[
          i,
        ]

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


cur_mean <- build_curves(
  wide = wide_mean,
  tag = "mean"
)

cur_min <- build_curves(
  wide = wide_min,
  tag = "min"
)

cur_max <- build_curves(
  wide = wide_max,
  tag = "max"
)


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
        x = c(
          df$x,
          rev(df$x),
          df$x[1]
        ),

        y = c(
          df$upper,
          rev(df$lower),
          df$upper[1]
        ),

        species = df$species[1],

        shoot_order = df$shoot_order[1]
      )
    }
  )


  do.call(
    rbind,
    closed_paths
  )
}


mean_path <- make_closed_path(
  cur_mean
)

min_path <- make_closed_path(
  cur_min
)

max_path <- make_closed_path(
  cur_max
)


## Restore factor levels after combining data frames

species_levels <- c(
  "M. intextum",
  "M. microcarpum"
)

shoot_order_levels <- c(
  "primary",
  "secondary"
)


mean_path <- mean_path %>%
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


min_path <- min_path %>%
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


max_path <- max_path %>%
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


## ------------------------------------------------------------
## 9. Define one shared scale bar
## ------------------------------------------------------------
##
## The scale bar is assigned specifically to the lower-left
## facet:
##
## M. intextum × secondary shoot leaves
##
## Supplying species and shoot_order prevents ggplot2 from
## repeating the scale bar in every facet.
## ------------------------------------------------------------

x_min_all <- min(
  max_path$x,
  na.rm = TRUE
)

y_min_all <- min(
  max_path$y,
  na.rm = TRUE
)


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

  x = (
    bar_x0 +
      bar_x1
  ) / 2,

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

  ## Maximum outline: black short-dashed line

  geom_path(
    data = max_path,

    aes(
      x = x,
      y = y,
      group = interaction(
        species,
        shoot_order
      )
    ),

    color = col_max,
    linewidth = lw_max,
    linetype = lt_max,
    lineend = "round",
    linejoin = "round"
  ) +


  ## Minimum outline: grey solid line

  geom_path(
    data = min_path,

    aes(
      x = x,
      y = y,
      group = interaction(
        species,
        shoot_order
      )
    ),

    color = col_min,
    linewidth = lw_min,
    linetype = lt_min,
    lineend = "round",
    linejoin = "round"
  ) +


  ## Mean outline: black solid line

  geom_path(
    data = mean_path,

    aes(
      x = x,
      y = y,
      group = interaction(
        species,
        shoot_order
      )
    ),

    color = col_mean,
    linewidth = lw_mean,
    linetype = lt_mean,
    lineend = "round",
    linejoin = "round"
  ) +


  ## Preserve the original faceted layout

  facet_grid(
    shoot_order ~ species
  ) +


  ## Preserve equal scaling in the x and y directions

  coord_equal() +


  ## Preserve the original print-safe theme

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


  ## One shared 0.5 mm scale bar

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


  ## Scale-bar label

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


## Display the plot

print(p)


## ------------------------------------------------------------
## 12. Save high-resolution TIFF
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


## ------------------------------------------------------------
## 13. Save vector PDF
## ------------------------------------------------------------

ggsave(
  filename = pdf_file,
  plot = p,
  width = 180,
  height = 110,
  units = "mm",
  bg = "white"
)


## ------------------------------------------------------------
## 14. Completion message
## ------------------------------------------------------------

cat(
  "\nFigure export completed.\n",
  "\nTIFF: ",
  tiff_file,
  "\nPDF: ",
  pdf_file,
  "\n",
  sep = ""
)
