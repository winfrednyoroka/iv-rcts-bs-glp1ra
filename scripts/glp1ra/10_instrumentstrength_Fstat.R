################################################################################
# Script for calculating the instrument strength, assessing relevance
# assumption.
# Calculate F-stat of BMI-difference post-intervention
################################################################################
# Load loibraries and functions
source('R/shared/setup.R')
source('R/glp1ra/calculate_fstatisticglp1ra.R')
source('R/shared/create_Fstatbubbleplot.R')
source('R/shared/plot_fstat.R')

# Read in the data
base_post <- readRDS('data/glp1ra/processed/baseline_post_relabelmultiarms.rds')
glimpse(base_post)

# Filter out the follow-up BMI only
bmi <- base_post |> 
  filter(Outcome == 'BMI' & post_time_months != 0) |> 
  filter(!is.na(sd)) # remove sd == 'NA', useful for calculating Fstat, if not present then it does not help me at all

glimpse(bmi)

controls <-
  bmi |>
  filter(treatment_group == "Control")

interventions <-
  bmi |>
  filter(treatment_group == "Intervention")
controls
interventions

# Left join the two datasets
fdat <-
  interventions |>
  left_join(
    controls,
    by = c("comparison_id", "post_time_months"),
    suffix = c("_int", "_ctrl")
  )
glimpse(fdat)


# fstat
fdat_fstat <- fdat |> 
  mutate(
    mean_diff = mean_int - mean_ctrl,
    
    se_diff =
      sqrt(
        sd_int^2 / samplesize_int +
          sd_ctrl^2 / samplesize_ctrl
      ),
    
    t_stat = mean_diff / se_diff,
    
    f_stat = t_stat^2
  )
glimpse(fdat_fstat)
bmi_fstat <- calc_fstat_multiarm(df = bmi,
           study_col = "comparison_id",
           time_col = "post_time_months",
           arm_col = "treatment_group",
           arm_name_col = 'arm_name',
           mean_col = "mean",
           sd_col = "sd",
           n_col = "samplesize")
glimpse(bmi_fstat)

################################
# Visualise the F-statistics
###############################
# Create year column and order in descending order
bmi_fstat <- bmi_fstat |> 
  mutate(
    Year = as.integer(
      stringr::str_extract(
        StudyID,
        "\\d{4}"
      )
    )
  ) |> 
  arrange(-Year)

bmi_fstat
# Filter to keep non-NAs
bmi_fstat <- bmi_fstat |> 
  filter(!is.na(mean_diff))
# Plot
fstatplot_size <- plot_fstat(data = bmi_fstat,
           study_col = "StudyID",
           duration_col = "StudyDuration",
           fstat_col = "f_stat",
           xlab = "Follow-up duration (months)",
           ylab = "")
fstatplot_size
############################
# Save the plot
############################
ggsave(
  filename = "output/glp1ra/figures/datadrivenvis/bmi_fstat/bmi_fstat_bubbleplot.jpeg",
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
saveRDS(bmi_fstat, 'data/glp1ra/processed/bmi_fstat.rds')
write_csv(bmi_fstat, 'output/glp1ra/tables/bmi_fstat.csv')




##################################
# Plot fstat colored----
##################################
glp1ra_fstatplot <- plot_fstat_colored(data = bmi_fstat,
           
           study_col = "StudyID",
           
           duration_col = "StudyDuration",
           
           fstat_col = "f_stat",
           
           f_threshold = 10,
           
           xlab = "Follow-up duration (months)",
           
           ylab = "")
glp1ra_fstatplot
############################
# Save the plot
############################
ggsave(
  filename = "output/glp1ra/figures/datadrivenvis/bmi_fstat/bmi_fstat_bubbleplotcolored.jpeg",
  plot = glp1ra_fstatplot,
  width = 12,      # inches
  height = 8,      # inches
  units = "in",
  dpi = 300
)
