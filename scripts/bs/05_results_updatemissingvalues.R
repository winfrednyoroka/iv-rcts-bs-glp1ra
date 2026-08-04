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
