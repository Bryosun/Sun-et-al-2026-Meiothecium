## =========================================================
## Figure 3: Meiothecium cell profile and violin plots
##
## Sun et al. (2026)
## Quantitative morphometrics reveal consistent shoot-order
## leaf differentiation and apical-to-basal laminal cell
## variation in two Meiothecium (Sematophyllaceae) species
## in Taiwan
##
## Accepted for publication in Hattoria
##
## Description:
## Generates Figure 3 showing:
## A. Mean cell length-to-width ratio profiles across leaf regions
## B. Violin plots, boxplots, and raw observations by leaf region
##
## Output:
## 600 dpi TIFF with LZW compression
## =========================================================

library(tidyverse)
library(readxl)
library(ragg)
library(patchwork)
library(grid)
library(systemfonts)

## ---------------------------------------------------------
## 1. File paths
## ---------------------------------------------------------

data_file <- file.path(
  "data",
  "Meiothecium_cell.xlsx"
)

out_file <- file.path(
  "figures",
  "Figure_3_Meiothecium.tiff"
)

if (!file.exists(data_file)) {
  stop(
    "Input file not found: ",
    data_file,
    "\nRun this script from the repository root directory."
  )
}

if (!dir.exists("figures")) {
  dir.create(
    "figures",
    recursive = TRUE
  )
}

## ---------------------------------------------------------
## 2. Check whether Arial is available
## ---------------------------------------------------------

arial_available <- system_fonts() %>%
  filter(
    str_detect(
      family,
      regex("^Arial$", ignore_case = TRUE)
    )
  ) %>%
  nrow() > 0

if (!arial_available) {
  warning(
    "Arial was not found in the system font list. ",
    "The exported figure may use a fallback sans-serif font."
  )
}

font_family <- "Arial"

## ---------------------------------------------------------
## 3. Read data
## ---------------------------------------------------------

cell <- read_excel(data_file)

## ---------------------------------------------------------
## 4. Clean column names and rename variables
## ---------------------------------------------------------

cell <- cell %>%
  rename_with(~ str_squish(.x)) %>%
  rename_with(~ str_replace_all(.x, " +", " ")) %>%
  rename(
    specimen = `vocher specimen`,
    region   = `position within the the leaf`,
    ratio    = `cell length-to-width ratio`
  )

## ---------------------------------------------------------
## 5. Define factor order
## ---------------------------------------------------------

region_levels <- c(
  "apical",
  "subapical",
  "middle",
  "basal",
  "supra_alar",
  "alar",
  "margin"
)

cell <- cell %>%
  mutate(
    region = as.character(region),

    region = str_replace_all(
      region,
      "[- ]",
      "_"
    ),

    region = factor(
      region,
      levels = region_levels
    ),

    species = factor(
      species,
      levels = c(
        "M. intextum",
        "M. microcarpum"
      )
    )
  ) %>%
  filter(
    !is.na(region),
    !is.na(ratio),
    !is.na(species)
  )

## ---------------------------------------------------------
## 6. Summary statistics
## ---------------------------------------------------------

sum_se <- cell %>%
  group_by(species, region) %>%
  summarise(
    n    = n(),
    mean = mean(ratio, na.rm = TRUE),
    sd   = sd(ratio, na.rm = TRUE),
    se   = sd / sqrt(n),
    .groups = "drop"
  )

## ---------------------------------------------------------
## 7. Graphic settings
## ---------------------------------------------------------

col_species <- c(
  "M. intextum"    = "#F8766D",
  "M. microcarpum" = "#00BFC4"
)

shape_species <- c(
  "M. intextum"    = 16,
  "M. microcarpum" = 17
)

line_species <- c(
  "M. intextum"    = "solid",
  "M. microcarpum" = "dashed"
)

region_labels <- c(
  "apical"     = "apical",
  "subapical"  = "subapical",
  "middle"     = "middle",
  "basal"      = "basal",
  "supra_alar" = "supra-alar",
  "alar"       = "alar",
  "margin"     = "margin"
)

species_labels_full <- c(
  "M. intextum" =
    expression(italic("M. intextum")),

  "M. microcarpum" =
    expression(italic("M. microcarpum"))
)

species_labels_short <- c(
  "M. intextum" =
    expression(italic("M. intex.")),

  "M. microcarpum" =
    expression(italic("M. micr."))
)

## ---------------------------------------------------------
## 8. Plot A: mean profiles and raw observations
## ---------------------------------------------------------

pA <- ggplot() +

  geom_point(
    data = cell,
    aes(
      x = region,
      y = ratio,
      color = species,
      shape = species
    ),
    position = position_jitter(
      width = 0.12,
      height = 0
    ),
    size = 2.5,
    alpha = 0.45
  ) +

  geom_line(
    data = sum_se,
    aes(
      x = region,
      y = mean,
      group = species,
      color = species,
      linetype = species
    ),
    linewidth = 1.3
  ) +

  geom_point(
    data = sum_se,
    aes(
      x = region,
      y = mean,
      color = species,
      shape = species
    ),
    size = 4.5
  ) +

  geom_errorbar(
    data = sum_se,
    aes(
      x = region,
      ymin = mean - se,
      ymax = mean + se,
      color = species
    ),
    width = 0.12,
    linewidth = 0.9
  ) +

  scale_color_manual(
    values = col_species,
    labels = species_labels_full
  ) +

  scale_shape_manual(
    values = shape_species,
    labels = species_labels_full
  ) +

  scale_linetype_manual(
    values = line_species,
    labels = species_labels_full
  ) +

  scale_x_discrete(
    labels = region_labels
  ) +

  labs(
    x = "Leaf region",
    y = "Cell L : W ratio",
    color = NULL,
    shape = NULL,
    linetype = NULL
  ) +

  coord_cartesian(
    clip = "off"
  ) +

  theme_bw(
    base_size = 17,
    base_family = font_family
  ) +

  theme(
    legend.position = c(0.76, 0.82),
    legend.justification = c(0.5, 0.5),
    legend.direction = "horizontal",

    legend.text = element_text(
      size = 15,
      family = font_family
    ),

    legend.key.width = unit(
      1.2,
      "lines"
    ),

    legend.key.height = unit(
      1.0,
      "lines"
    ),

    legend.background = element_rect(
      fill = "white",
      color = NA
    ),

    legend.key = element_rect(
      fill = "white",
      color = NA
    ),

    legend.margin = margin(
      t = 2,
      r = 3,
      b = 2,
      l = 3
    ),

    legend.box.margin = margin(
      0,
      0,
      0,
      0
    ),

    axis.title.x = element_text(
      size = 16,
      family = font_family,
      margin = margin(
        t = 8
      )
    ),

    axis.title.y = element_text(
      size = 16,
      family = font_family,
      margin = margin(
        r = 6
      )
    ),

    axis.text.x = element_text(
      size = 15,
      family = font_family,
      angle = 30,
      hjust = 1,
      vjust = 1
    ),

    axis.text.y = element_text(
      size = 15,
      family = font_family
    ),

    plot.margin = margin(
      t = 8,
      r = 10,
      b = 6,
      l = 10
    )
  )

## ---------------------------------------------------------
## 9. Plot B: violin plots, boxplots and raw observations
## ---------------------------------------------------------

pB <- ggplot(
  cell,
  aes(
    x = species,
    y = ratio,
    fill = species
  )
) +

  geom_violin(
    trim = TRUE,
    alpha = 0.28,
    color = NA,
    width = 0.65
  ) +

  geom_boxplot(
    width = 0.12,
    outlier.shape = NA,
    alpha = 0.75,
    color = "black",
    linewidth = 0.6
  ) +

  geom_point(
    aes(
      shape = species
    ),
    position = position_jitter(
      width = 0.045,
      height = 0
    ),
    size = 2.0,
    alpha = 0.65,
    color = "black"
  ) +

  facet_wrap(
    ~ region,
    ncol = 4,
    scales = "free_y",
    labeller = as_labeller(
      region_labels
    )
  ) +

  scale_fill_manual(
    values = col_species
  ) +

  scale_shape_manual(
    values = shape_species
  ) +

  scale_x_discrete(
    labels = species_labels_short,
    expand = expansion(
      add = 0.55
    )
  ) +

  scale_y_continuous(
    expand = expansion(
      mult = c(
        0.03,
        0.06
      )
    )
  ) +

  labs(
    x = NULL,
    y = "Cell L : W ratio"
  ) +

  theme_bw(
    base_size = 16,
    base_family = font_family
  ) +

  theme(
    legend.position = "none",

    strip.background = element_rect(
      fill = "grey85",
      color = "black",
      linewidth = 0.6
    ),

    strip.text = element_text(
      face = "plain",
      size = 15,
      family = font_family,
      margin = margin(
        t = 5,
        r = 2,
        b = 5,
        l = 2
      )
    ),

    axis.title.y = element_text(
      size = 16,
      family = font_family,
      margin = margin(
        r = 6
      )
    ),

    axis.text.y = element_text(
      size = 14,
      family = font_family
    ),

    axis.text.x = element_text(
      size = 13,
      family = font_family,
      margin = margin(
        t = 6
      )
    ),

    axis.ticks.x = element_blank(),

    panel.spacing.x = unit(
      1.0,
      "lines"
    ),

    panel.spacing.y = unit(
      1.2,
      "lines"
    ),

    plot.margin = margin(
      t = 6,
      r = 10,
      b = 8,
      l = 10
    )
  )

## ---------------------------------------------------------
## 10. Combine Plot A and Plot B
## ---------------------------------------------------------

final_plot <- pA / pB +

  plot_layout(
    heights = c(
      1.10,
      1.75
    )
  ) +

  plot_annotation(
    tag_levels = "A"
  ) &

  theme(
    plot.tag = element_text(
      face = "bold",
      size = 22,
      family = font_family
    )
  )

## ---------------------------------------------------------
## 11. Export TIFF
## ---------------------------------------------------------

agg_tiff(
  filename = out_file,
  width = 10.5,
  height = 9.2,
  units = "in",
  res = 600,
  compression = "lzw",
  background = "white"
)

print(final_plot)

dev.off()

message(
  "Figure saved to: ",
  out_file
)
