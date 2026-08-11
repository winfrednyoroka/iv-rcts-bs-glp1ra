################################################################################
# Script for calculating the instrument strength, assessing relevance
# assumption.
# Calculate F-stat of BMI-difference post-intervention
################################################################################
# Load loibraries and functions
source('R/shared/setup.R')
source('R/shared/calculate_fstatistic.R')

# Read in the data
base_post_combined <- readRDS('data/bs/processed/baseline_post_combinedmultiarms.rds')
glimpse(base_post_combined)

# Filter out the follow-up BMI only
bmi <- base_post_combined |> 
  filter(Outcome == 'BMI' & post_time_months != 0) |> 
  filter(!is.na(sd)) # remove sd == 'NA', useful for calculating Fstat, if not present then it does not help me at all

bmi
bmi_fstat <- calc_fstat(df = bmi,
           study_col = "study_id",
           time_col = "post_time_months",
           arm_col = "treatment_group",
           mean_col = "mean",
           sd_col = "sd",
           n_col = "samplesize")
bmi_fstat
###################################################
# Save RDS object of the F-stat
# Write a csv output in table folder
####################################################

# Visualise the F-stat
fstatplot_size <- ggplot(
  bmi_fstat,
  aes(
    x = StudyDuration,
    y = StudyID,
    size = f_stat
  )
) +
  geom_point(
    alpha = 0.5,
    shape = 16
  ) +
  scale_size_continuous(
    name = "F-statistic",
    range = c(2, 12)
  ) +
  scale_x_continuous(
    breaks = sort(unique(bmi_fstat$StudyDuration))
  ) +
  labs(
    x = "Follow-up duration (months)",
    y = "Author(Year)",
    #title = "Instrument strength by study and follow-up duration"
  ) +
  theme_classic()

fstatplot_size
############################
# Save the plot
############################
ggsave(
  filename = "output/bs/figures/bmi_fstat_bubbleplot.jpeg",
  plot = fstatplot_size,
  width = 12,      # inches
  height = 8,      # inches
  units = "in",
  dpi = 300
)
