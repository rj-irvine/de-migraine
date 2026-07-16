###############################################################################
# Study Name           : UK Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-20
#
# Program Inputs       : "data/patpop_matched"
# Program Outputs      : "data/cov3"
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2025-11-20  Ryan Irvine             Conversion from SAS to R
# 0.2
# 1.0
################################################################################

# Step 1. Run global program ----
source("R/00_global.R")

data <- readRDS("data/gp_visit_annual")

ggplot(data, aes(x = n_visit_annual, fill = cohort)) +
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


ggplot(data, aes(x = cohort, y = n_visit_annual, color = cohort)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_color_manual(
    values = c("case" = "#1f78b4", "control" = "#e31a1c"),
    name = "Cohort",
    labels = c("Case", "Control")
  ) +
  labs(
    title = "Figure 1a. Distribution of Annualized GP Visits by Cohort",
    x = "Cohort",
    y = "Annualized Number of Visits"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
