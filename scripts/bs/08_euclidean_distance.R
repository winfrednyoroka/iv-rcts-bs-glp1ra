##########################################################################################
# Script for calculating Euclidean distance in  RCT arms----
# Reads in the data consisting of baseline and follow up
# 
##########################################################################################
# Load the libraries and functions
source('R/shared/setup.R')
source('R/shared/plot_euclidean_distance.R')

# Read in the data
baseline_post <- readRDS('data/bs/processed/baseline_post_relabelmultiarms.rds')
glimpse(baseline_post)

# Filter and keep baseline measures only at time point 0
comparison_data <- baseline_post |>
  filter(post_time_months == 0) |>
  select(comparison_id, author_year, Outcome,
         treatment_group, mean) |>
  tidyr::pivot_wider(
    names_from = c(Outcome, treatment_group),
    values_from = mean
  ) |>
  mutate(
    bmi_diff = scale(BMI_Intervention)[,1] -
      scale(BMI_Control)[,1],
    
    sbp_diff = scale(SBP_Intervention)[,1] -
      scale(SBP_Control)[,1],
    
    dbp_diff = scale(DBP_Intervention)[,1] -
      scale(DBP_Control)[,1],
    
    euclidean_bmi_sbp_dbp =
      sqrt(
        bmi_diff^2 +
          sbp_diff^2 +
          dbp_diff^2
      )
  )
glimpse(comparison_data)

######################
comparison_data <- comparison_data |>
  mutate(
    euclidean_bmi_sbp_dbp =
      sqrt(bmi_diff^2 + sbp_diff^2 + dbp_diff^2),
    
    euclidean_bmi_sbp =
      sqrt(bmi_diff^2 + sbp_diff^2),
    
    euclidean_bmi_dbp =
      sqrt(bmi_diff^2 + dbp_diff^2)
  )
glimpse(comparison_data)

#################################################
# Visualise bmi_sbp_dbp Euclidean distance----
#################################################
euc_bmi_sbp_dbp <- comparison_data |> 
  filter(!is.na(euclidean_bmi_sbp_dbp)) 

euc_bmi_sbp_dbp_plot <- plot_euclidean_distance(data = euc_bmi_sbp_dbp,
                        x_var = "euclidean_bmi_sbp_dbp",
                        y_var = "comparison_id",
                        point_size = 5,
                        point_colour = "gray40",
                        xlabel = "Euclidean Distance",
                        ylabel = "Author (Year) (Comparisons)")
#########################################
## Save bmi_sbp_dbp euc_dist plot-----
########################################

ggsave(filename = "output/bs/figures/datadrivenvis/euc_dist/bmi_sbp_dbpEucdist.jpeg",
  plot = euc_bmi_sbp_dbp_plot,
  width = 12,      # inches
  height = 8,      # inches
  units = "in",
  dpi = 300)
#################################################
# Visualise bmi_sbp Euclidean distance----
#################################################
euc_bmi_sbp <- comparison_data |> 
  filter(!is.na(euclidean_bmi_sbp)) 

euc_bmi_sbp_plot <- plot_euclidean_distance(data = euc_bmi_sbp,
                        x_var = "euclidean_bmi_sbp",
                        y_var = "comparison_id",
                        point_size = 5,
                        point_colour = "gray40",
                        xlabel = "Euclidean Distance",
                        ylabel = "Author (Year) (Comparisons)")
#########################################
## Save bmi_sbp_dbp euc_dist plot-----
########################################

ggsave(filename = "output/bs/figures/datadrivenvis/euc_dist/bmi_sbpEucdist.jpeg",
       plot = euc_bmi_sbp_plot,
       width = 12,      # inches
       height = 8,      # inches
       units = "in",
       dpi = 300)

#################################################
# Visualise bmi_dbp Euclidean distance----
#################################################
euc_bmi_dbp <- comparison_data |> 
  filter(!is.na(euclidean_bmi_dbp)) 

euc_bmi_dbp_plot <- plot_euclidean_distance(data = euc_bmi_dbp,
                                            x_var = "euclidean_bmi_dbp",
                                            y_var = "comparison_id",
                                            point_size = 5,
                                            point_colour = "gray40",
                                            xlabel = "Euclidean Distance",
                                            ylabel = "Author (Year) (Comparisons)")
#########################################
## Save bmi_dbp euc_dist plot-----
########################################

ggsave(filename = "output/bs/figures/datadrivenvis/euc_dist/bmi_dbpEucdist.jpeg",
       plot = euc_bmi_dbp_plot,
       width = 12,      # inches
       height = 8,      # inches
       units = "in",
       dpi = 300)
