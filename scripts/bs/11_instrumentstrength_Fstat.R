################################################################################
# Script for calculating the instrument strength, assessing relevance
# assumption.
# Calculate F-stat of BMI-difference post-intervention
################################################################################
# Load loibraries and functions
source('R/shared/setup.R')
source('R/shared/calculate_fstatistic.R')
source('R/shared/create_Fstatbubbleplot.R')

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

################################
# Visualise the F-statistics
###############################
# Create year column and order in desceding order
bmi_fstat <- bmi_fstat %>%
  mutate(
    Year = as.integer(
      stringr::str_extract(
        StudyID,
        "\\d{4}"
      )
    )
  ) %>%
  arrange(-Year)

bmi_fstat

# Plot
fstatplot_size <- plot_fstat(data = bmi_fstat,
           study_col = "StudyID",
           duration_col = "StudyDuration",
           fstat_col = "f_stat",
           xlab = "Follow-up duration (months)",
           ylab = "")
############################
# Save the plot
############################
ggsave(
  filename = "output/bs/figures/datadrivenvis/bmi_fstat/bmi_fstat_bubbleplot.jpeg",
  plot = fstatplot_size,
  width = 12,      # inches
  height = 8,      # inches
  units = "in",
  dpi = 300
)
###################################################
# Save RDS object of the F-stat
# Write a csv output in table folder
####################################################
saveRDS(bmi_fstat, 'data/bs/processed/bmi_fstat.rds')
write_csv(bmi_fstat, 'output/bs/tables/bmi_fstat.csv')
