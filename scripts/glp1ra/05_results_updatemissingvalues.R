##########################################################################################
# Script for updating the results.
# Firstly the script reads in the results from post (actual means) reported
# post-intervention and the results based on change from baseline
# Secondly uses imputestats function to update missing values in change from baseline
# Thirdly, join change from baseline results with baseline, and calculate the actual means
# Fourthly, update the CIs, SDs and SEs for the results (post- actual reported means)
# Combine with the baseline to obtain the author name and year of publication
# Finally combine the updated post with updated change and save as rds object
#########################################################################################
#################################################
# Load libraries and custom functions----
#################################################
source("R/shared/setup.R")
source("R/shared/imputechangestats.R")
source("R/shared/update_missing_CIs.R")
source("R/shared/update_missing_SDs_SEs.R")
source("R/shared/update_results_fromchangeandbaseline.R")

####################################################
# Read in the data ----
###################################################
baseline <- readRDS("data/glp1ra/processed/Study_trialarms_baseline_CIsSDsSEs_updated.rds")
glimpse(baseline)
change <- readRDS("data/glp1ra/processed/change.rds")
glimpse(change)
post <- readRDS("data/glp1ra/processed/post.rds")
glimpse(post)

##############################################################
# Update and impute missing change from baseline values----
# Use Imputestats function
#############################################################
change_updated <- impute_change_stats(
  data = change, mean_col = "change_mean", lower_col = "change_lowerbound", upper_col = "change_upperbound", sd_col = "change_sd",
  se_col = "change_se", n_col = "change_samplesize", ci_level_col = "change_CI_level", default_ci = 95
)
glimpse(change_updated)

##############################################################
# Join baseline and change_updated ----
# Calculate the results form baseline and change_updated
# use update_results_fromchangeandbaseline function
#############################################################
change_baseline <- change_updated |>
  left_join(baseline, by = c("ARMID", "Outcome"))
glimpse(change_baseline)
results_change_baseline <- create_results_values(
  data = change_baseline, baseline_mean = baseline_mean, baseline_sd = baseline_sd,
  mean_change = change_mean, sd_change = change_sd, sample_size = change_samplesize,
  r = .7, prefix = "post"
)
glimpse(results_change_baseline)

##############################################################
# Update results; post intervention mean values----
# Use update CI and calc sd and se functions
#############################################################
glimpse(baseline)
glimpse(post)
######################################################
# Fill in the CIs, SDs and SEs where necessary----
# Use the update_confidence_bounds function
# Use the Calc_SD_SE, to update the SDs and SEs
######################################################
post_ci <- update_confidence_bounds(data = post,
                                        mean_col = "post_mean",
                                        standard_deviation_col = "post_sd",
                                        standard_error_col = "post_se",
                                        sample_size_col = "post_samplesize",
                                        lower_bound_col = "post_lowerbound",
                                        upper_bound_col = "post_upperbound",
                                        z = 1.96,
                                        digits = 2)
glimpse(post_ci)

#################################
# Update the SDs and SEs ----
#################################

post_ci_sd_se <- calc_sd_se_from_ci( data = post_ci,
                                     lower_col = post_lowerbound,
                                     upper_col = post_upperbound,
                                     n_col = post_samplesize,
                                     se_col = post_se,
                                     sd_col = post_sd,
                                     ci_level = 0.95)
glimpse(post_ci_sd_se)

##############################################
# Left join baseline to results----
##############################################
glimpse(baseline)
post_baseline <- post_ci_sd_se |> 
  left_join(baseline, by = c('ARMID', 'Outcome'))
glimpse(post_baseline) # 377 rows

#########################################################################
# Row bind the results of change from baseline and the post results----
########################################################################
# Select vital columns from updated change and post
results_change_baseline <- results_change_baseline |> 
  rename('ID' = 'ID.x', 'post_time_months' = 'change_time_months', 'post_samplesize' = 'change_samplesize') |> 
  select('ID', 'ARMID','post_time_months', 'age', 'Outcome','treatment_group','female_pct', 'baseline_mean', 'baseline_lowerbound', 
         'baseline_upperbound', 'baseline_sd', 'baseline_se', 'baseline_N_per_arm', 'last_name', 'publication_year',
         'post_mean','post_lowerbound', 'post_upperbound', 'post_sd', 'post_se','post_samplesize')
glimpse(results_change_baseline) # 523 X 21

post_baseline <- post_baseline |> 
  rename('ID' = 'ID.x') |> 
  select(
    'ID', 'ARMID', 'post_time_months', 'age', 'Outcome', 'treatment_group',
    'female_pct', 'baseline_mean', 'baseline_lowerbound', 'baseline_upperbound',
    'baseline_sd', 'baseline_se', 'baseline_N_per_arm', 'last_name',
    'publication_year', 'post_mean', 'post_lowerbound', 'post_upperbound',
    'post_sd', 'post_se', 'post_samplesize'
  )
glimpse(post_baseline) # 56 X 21

# Row bind the change and post 
rows_to_add <- results_change_baseline |> 
  anti_join(
    post_baseline,
    by = c("ID", "Outcome", 'post_time_months', "ARMID" ) )

all_post_results <- bind_rows(
  post_baseline,
  rows_to_add
)

# Final results ------
glimpse(all_post_results) # 535 rows
all_post_results$Outcome

###############################################################
#  Save all updated results to be used by the next script----
##############################################################
saveRDS(all_post_results, file = 'data/glp1ra/processed/post_results_updated.rds')


