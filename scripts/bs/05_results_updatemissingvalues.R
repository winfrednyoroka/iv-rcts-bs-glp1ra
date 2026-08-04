##########################################################################################
# Script for updating the results. 
# Firstly the script reads in the results from post (actual means) reported 
# post-intervention and the results based on change from baseline
# Secondly uses imputestats function to update missing values in change from baseline
# Thirdly, join change with baseline, and calculate the actual means
# Fourthly, update the CIs, SDs and SEs for the results (post- actual reported means)
# Combine with the baseline to obtain the author name and year of publication
# Finally combine the updated post with updated change and save as rds object
#########################################################################################
#################################################
# Load libraries and custom functions----
#################################################
source('R/shared/setup.R')
source('R/shared/imputechangestats.R')
source('R/shared/update_missing_CIs.R')
source('R/shared/update_missing_SDs_SEs.R')

####################################################
# Read in the data ----
###################################################
baseline <- readRDS('data/bs/processed/Study_trialarms_baseline_CIsSDsSEs_updated.rds')
glimpse(baseline)
change <- readRDS('data/bs/processed/change.rds')
glimpse(change)
post <- readRDS('data/bs/processed/post.rds')
glimpse(post)

##############################################################
# Update and impute missing change from baseline values----
# Use Imputestats function
#############################################################
change_updated <- impute_change_stats(data = change,mean_col = 'change_mean', lower_col = 'change_lowerbound', upper_col = 'change_upperbound',sd_col = 'change_sd',
                      se_col = 'change_se',n_col = 'change_samplesize', ci_level_col = 'change_CI_level', default_ci = 95)
glimpse(change_updated)

##############################################################
# Join baseline and change_updated ----
# Calculate the results form baseline and change_updated
#############################################################
change_baseline <- change_updated |> 
  left_join(baseline, by = c('ARMID','Outcome'))
glimpse(change_baseline)
