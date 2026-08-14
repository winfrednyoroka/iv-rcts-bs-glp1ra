################################################################################
# Script for visualising BMI vs BP baseline vs post-intervention
# Baseline, 12 months and 24 months plots with CIs
# Baseline and all time points post-intervention with confidence intervals
################################################################################
# Load libraries where necessary
source('R/shared/setup.R')
source('R/shared/plot_baseline_followupmeansandCIs.R')
# Read in the data
baseline_post <- read_rds('data/bs/processed/baseline_post_relabelmultiarms.rds')
glimpse(baseline_post)

# Create BMI-SBP and BMI-DBP for plotting

##########################################################
# Create BMI_SBP-----
##########################################################
bmi_sbp_data <-
  
  baseline_post |> 
  
  group_by(comparison_id) |> 
  
  filter(
    all(c("BMI", "SBP") %in% Outcome)
  ) |> 
  
  ungroup()
# Reshape for visualisation
bmi_sbp_plot <- bmi_sbp_data |> 
  select(
    comparison_id,
    author_year,
    arm_name,
    treatment_group,
    post_time_months ,
    Outcome,
    mean,
    lowerbound,
    upperbound
  ) |> 
  group_by(
    comparison_id,
    author_year,
    arm_name,
    treatment_group,
    post_time_months,
    Outcome
  ) |> 
  summarise(
    mean = first(mean),
    lowerbound = first(lowerbound),
    upperbound = first(upperbound),
    .groups = "drop"
  ) |> 
  pivot_wider(
    names_from = Outcome,
    values_from = c(
      mean,
      lowerbound,
      upperbound
    )
  )
glimpse(bmi_sbp_plot)
# Prep the data for visualisation
bmi_sbp_plot <- bmi_sbp_plot |> 
  mutate(
    month_f = factor(
      post_time_months ,
      levels = sort(unique(post_time_months ))
    )
  )
bmi_sbp_plot <- bmi_sbp_plot |> 
  filter(
    !is.na(mean_BMI),
    !is.na(mean_SBP)
  )

##########################################################
# Create BMI-DBP----
##########################################################
bmi_dbp_data <-
  
 baseline_post |> 
  
  group_by(comparison_id) |> 
  
  filter(
    all(c("BMI", "DBP") %in% Outcome)
  ) |> 
  
  ungroup()
# Reshape for visualisation
bmi_dbp_plot <- bmi_dbp_data |> 
  select(
    comparison_id,
    author_year,
    arm_name,
    treatment_group,
    post_time_months,
    Outcome,
    mean,
    lowerbound,
    upperbound
  ) |> 
  group_by(
    comparison_id,
    author_year,
    arm_name,
    treatment_group,
    post_time_months,
    Outcome
  ) |> 
  summarise(
    mean = first(mean),
    lowerbound = first(lowerbound),
    upperbound = first(upperbound),
    .groups = "drop"
  ) |> 
  pivot_wider(
    names_from = Outcome,
    values_from = c(
      mean,
      lowerbound,
      upperbound
    )
  )
glimpse(bmi_dbp_plot)
# Prep the data for visualisation
bmi_dbp_plot <- bmi_dbp_plot |> 
  mutate(
    month_f = factor(
      post_time_months,
      levels = sort(unique(post_time_months))
    )
  )
bmi_dbp_plot <- bmi_dbp_plot |> 
  filter(
    !is.na(mean_BMI),
    !is.na(mean_DBP)
  )
glimpse(bmi_dbp_plot)

# Visualisation----
#############################################
# BMI-SBP plot ----
# All time points individual plots
#############################################
bmi_sbp <- plot_bmi_bp_trajectory(data = bmi_sbp_plot,
                       x_mean = mean_BMI,
                       x_lower = lowerbound_BMI,
                       x_upper = upperbound_BMI,
                       y_mean = mean_SBP,
                       y_lower = lowerbound_SBP,
                       y_upper = upperbound_SBP,
                       facet_var = comparison_id,
                       group_var = arm_name,
                       colour_var = treatment_group,
                       shape_var = month_f,
                       point_size = 1.5,
                       bmi_ref = 25,
                       bp_ref = 120,
                       scales = "fixed",
                       ci_alpha = 0.35,
                       x_label = expression("BMI (kg/m"^2*")"),
                       y_label = 'Systolic blood pressure (mmHg)',
                       shape_label = "Months",
                       save_individual = TRUE,
                       output_dir = "output/bs/figures/datadrivenvis/sbp_all_time_points/",
                       consistent_limits = FALSE,
                       width = 8,
                       height = 6,
                       facet_wrap_width = 20,
                       title_wrap_width = 100)
bmi_sbp
###########################
# Save the plot-------
###########################
ggsave(
  filename = "output/bs/figures/bmi_sbp_base_post_ci_alltimepoints.jpeg",
  plot = bmi_sbp,
  width = 8,      # inches
  height = 6,      # inches
  units = "in",
  dpi = 300
)

#############################################
# BMI-DBP plot -----
# All time points individual plots
#############################################
bmi_dbp <- plot_bmi_bp_trajectory(data = bmi_dbp_plot,
                       x_mean = mean_BMI,
                       x_lower = lowerbound_BMI,
                       x_upper = upperbound_BMI,
                       y_mean = mean_DBP,
                       y_lower = lowerbound_DBP,
                       y_upper = upperbound_DBP,
                       facet_var = comparison_id,
                       group_var = arm_name,
                       colour_var = treatment_group,
                       shape_var = month_f,
                       point_size = 3,
                       bmi_ref = 25,
                       bp_ref = 80,
                       scales = "fixed",
                       ci_alpha = 0.35,
                       x_label = expression("BMI (kg/m"^2*")"),
                       y_label = 'Diastolic blood pressure (mmHg)',
                       shape_label = "Months",
                       save_individual = TRUE,
                       output_dir = "output/bs/figures/datadrivenvis/dbp_all_time_points/",
                       consistent_limits = FALSE,
                       width = 8,
                       height = 6,
                       facet_wrap_width = 20,
                       title_wrap_width = 100)
###########################
# Save the plot-------
###########################
ggsave(
  filename = "output/bs/figures/bmi_dbp_base_post_ci_alltimepoints.jpeg",
  plot = bmi_dbp,
  width = 8,      # inches
  height = 6,      # inches
  units = "in",
  dpi = 300
)


##############################################################################
# Plot BMI-SBP at baseline, 12 and 24 months only-----
##############################################################################
target_months <- c(12,24)

bmi_sbp_12_24 <- bmi_sbp_plot |>
  group_by(comparison_id, arm_name) |>
  filter(any(post_time_months %in% target_months)) |>
  filter(post_time_months %in% c(0, target_months)) |>
  ungroup()
#bmi_sbp_12_24 <- bmi_sbp_plot |> 
  #filter(post_time_months == 0 | post_time_months == 12 | post_time_months ==24)
bmi_sbp_12_24
#############################################
# BMI-SBP plot 
# All time points individual plots
#############################################
bmi_sbp_12_24_plot <- plot_bmi_bp_trajectory(data = bmi_sbp_12_24,
                                  x_mean = mean_BMI,
                                  x_lower = lowerbound_BMI,
                                  x_upper = upperbound_BMI,
                                  y_mean = mean_SBP,
                                  y_lower = lowerbound_SBP,
                                  y_upper = upperbound_SBP,
                                  facet_var = comparison_id,
                                  group_var = arm_name,
                                  colour_var = treatment_group,
                                  shape_var = month_f,
                                  point_size = 1.5,
                                  bmi_ref = 25,
                                  bp_ref = 120,
                                  scales = "fixed",
                                  ci_alpha = 0.35,
                                  x_label = expression("BMI (kg/m"^2*")"),
                                  y_label = 'Systolic blood pressure (mmHg)',
                                  shape_label = "Months",
                                  save_individual = TRUE,
                                  output_dir = "output/bs/figures/datadrivenvis/sbp_12and24months/",
                                  consistent_limits = FALSE,
                                  width = 8,
                                  height = 6,
                                  facet_wrap_width = 20,
                                  title_wrap_width = 100)
###########################
# Save the plot-------
###########################
ggsave(
  filename = "output/bs/figures/bmi_sbp_base_post_ci_12and24months.jpeg",
  plot = bmi_sbp_12_24_plot,
  width = 8,      # inches
  height = 6,      # inches
  units = "in",
  dpi = 300
)

##############################################################################
# Plot BMI-DBP at baseline, 12 and 24 months only-----
##############################################################################
bmi_dbp_12_24 <- bmi_dbp_plot |> 
  filter(post_time_months == 0 | post_time_months == 12 | post_time_months ==24)

bmi_dbp_12_24 <- bmi_dbp_plot |>
  group_by(author_year) |>
  filter(
    any(post_time_months %in% c(12, 24))
  ) |>
  ungroup() |>
  filter(post_time_months %in% c(0, 12, 24))

#############################################
# BMI-DBP plot 
# All time points individual plots
#############################################
bmi_dbp_12_24_plot <- plot_bmi_bp_trajectory(data = bmi_dbp_12_24,
                                             x_mean = mean_BMI,
                                             x_lower = lowerbound_BMI,
                                             x_upper = upperbound_BMI,
                                             y_mean = mean_DBP,
                                             y_lower = lowerbound_DBP,
                                             y_upper = upperbound_DBP,
                                             facet_var = comparison_id,
                                             group_var = arm_name,
                                             colour_var = treatment_group,
                                             shape_var = month_f,
                                             point_size = 2,
                                             bmi_ref = 25,
                                             bp_ref = 80,
                                             scales = "fixed",
                                             ci_alpha = 0.35,
                                             x_label = expression("BMI (kg/m"^2*")"),
                                             y_label = 'Diastolic blood pressure (mmHg)',
                                             shape_label = "Months",
                                             save_individual = TRUE,
                                             output_dir = "output/bs/figures/datadrivenvis/dbp_12and24months/",
                                             consistent_limits = FALSE,
                                             width = 8,
                                             height = 6,
                                             facet_wrap_width = 20,
                                             title_wrap_width = 100)
###########################
# Save the plot-------
###########################
ggsave(
  filename = "output/bs/figures/bmi_dbp_base_post_ci_12and24months.jpeg",
  plot = bmi_dbp_12_24_plot,
  width = 8,      # inches
  height = 6,      # inches
  units = "in",
  dpi = 300
)
