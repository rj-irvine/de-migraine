###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-20
#
# Program Inputs       : "data/gp_visit_annual"
# Program Outputs      : "results/figure1a_gp_visits_hist.png",
#                        "results/figure1b_gp_visits_jitter.png"
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2025-11-20  Ryan Irvine             Conversion from SAS to R
# 0.2       2026-07-20  Ryan Irvine             DE port: fix source path, save
#                                               figures to results/
# 1.0
################################################################################

# Step 1. Run global program ----
source("00_global.R")

data <- readRDS("data/gp_visit_annual")

# Figure 1a. Distribution of annualized GP visits (overlaid histogram) ----
fig1a <- ggplot(data, aes(x = n_visit_annual, fill = cohort)) +
  geom_histogram(
    position = "identity", # overlay histograms
    alpha = 0.6, # transparency so both cohorts are visible
    binwidth = 1 # adjust binwidth to suit your variable scale
  ) +
  scale_fill_manual(
    values = c("case" = "#1f78b4", "control" = "#e31a1c"), # match actual cohort values
    name = "Cohort",
    labels = c("Case", "Control") # nice legend labels
  ) +
  labs(
    title = "Figure 1a. Distribution of Annualized GP Visits by Cohort",
    x = "Annualized Number of Visits",
    y = "Count"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave(
  "results/figure1a_gp_visits_hist.png",
  fig1a,
  width = 8, height = 5, dpi = 300, bg = "white"
)

# Figure 1b. Annualized GP visits by cohort (jitter) ----
fig1b <- ggplot(data, aes(x = cohort, y = n_visit_annual, color = cohort)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_color_manual(
    values = c("case" = "#1f78b4", "control" = "#e31a1c"),
    name = "Cohort",
    labels = c("Case", "Control")
  ) +
  labs(
    title = "Figure 1b. Distribution of Annualized GP Visits by Cohort",
    x = "Cohort",
    y = "Annualized Number of Visits"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave(
  "results/figure1b_gp_visits_jitter.png",
  fig1b,
  width = 8, height = 5, dpi = 300, bg = "white"
)

print("Figure 1a/1b have been written to the results directory.")
