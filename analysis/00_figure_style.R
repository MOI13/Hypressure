# ==============================================================================
# FIGURE STYLE
# Journal-style figures shared by the reports:
# no grid lines, no panel border, thin axis lines, legend at the bottom,
# one color palette per stratum (sex, age group, BMI category) used for every figure.
# ==============================================================================

# Colors ----------------------------------------------------------------------
col_main   <- "grey20"
col_ref    <- "indianred2"
col_grey   <- "grey40"
col_female <- "indianred2"
col_male   <- "royalblue"

sex_colors <- c("Female" = col_female, "Male" = col_male)

## ordered groups get a light-to-dark scale
age_group_colors <- c(
  "18-29 years" = "#6baed6", "30-39 years" = "#4292c6", "40-49 years" = "#2171b5",
  "50-59 years" = "#08519c", "60+ years"   = "#08306b"
)

bmi_colors <- c(
  "Underweight" = "#fdae6b", "Normal weight" = "#fd8d3c", 
  "Overweight"  = "#e6550d", "Obesity"       = "#a63603"
)

## BP medication start (dashboard): grey for never, dark to light purple from entry to year 6
med_start_colors <- c(
  "Never"     = "grey50",  "At entry"  = "#3f007d", "At year 1" = "#54278f", "At year 2" = "#6a51a3",
  "At year 3" = "#807dba", "At year 4" = "#9e9ac8", "At year 5" = "#a8a6d1", "At year 6" = "#bcbddc"
)

## one palette per stratum, used by every figure in the reports and the dashboard
strata_colors <- list(
  overall      = c("All participants" = col_main),
  sex          = sex_colors,
  age_group    = age_group_colors,
  bmi_category = bmi_colors,
  med_start    = med_start_colors
)

scale_color_strata <- function(group_var) {
  scale_color_manual(values = strata_colors[[group_var]], breaks = names(strata_colors[[group_var]]))
}

scale_fill_strata <- function(group_var) {
  scale_fill_manual(values = strata_colors[[group_var]], breaks = names(strata_colors[[group_var]]))
}


# ggplot2 ---------------------------------------------------------------------
theme_journal <- function(base_size = 11) {
  theme_classic(base_size = base_size, base_family = "sans") %+replace%
    theme(
      ## no grid lines or panel border, thin axis lines
      panel.grid       = element_blank(),
      panel.border     = element_blank(),
      axis.line        = element_line(linewidth = 0.4, color = "black"),
      axis.ticks       = element_line(linewidth = 0.4, color = "black"),
      axis.text        = element_text(color = "black"),
      ## facet labels as plain bold text, no box
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", hjust = 0, margin = margin(b = 4)),
      ## legend below the plot, no box
      legend.position  = "bottom",
      legend.background = element_blank(),
      legend.key       = element_blank(),
      legend.title     = element_blank(),
      ## space between panel titles and the plot, and around each panel
      plot.title       = element_text(face = "bold", hjust = 0, size = base_size, margin = margin(b = 12)),
      plot.title.position = "plot",
      plot.margin      = margin(t = 12, r = 12, b = 8, l = 8),
      plot.background  = element_blank()
    )
}

theme_set(theme_journal())

## thin lines and small points by default
update_geom_defaults("line",   list(linewidth = 0.7))
update_geom_defaults("point",  list(size = 1.6))
update_geom_defaults("smooth", list(linewidth = 0.9))


# Base R ----------------------------------------------------------------------
## for plots drawn with plot()/ plot.cox.zph(): L-shaped axis, no box, horizontal tick labels
## usage: par(mfrow = c(2, 2), mar = ..., oma = ..., bty = "l", las = 1)
base_par <- list(bty = "l", las = 1, tcl = -0.3)