##########################################################################################
# Script for filling in missing confidence bounds (lower bound, upper bound)
# using standard error (SE), standard deviation (SD) and sample size where possible
# and sample size can not be zero. samplesize is in studysheet_trialarms_updated.rds.
# Read in the data here and left join to ensure you have sample size
# Writes the updated data in a labelled rds file in the data/bs/processed folder
#########################################################################################
#################################################
# Load libraries and custom functions----
#################################################
source('R/shared/setup.R')
source('R/shared/update_missing_CIs.R')
source('R/shared/update_missing_SDs_SEs.R')
########################
# Read in the data ----
#######################
# Read the baseline data (BMI, SBP,DBP,HTN)
baseline <- readRDS('data/glp1ra/processed/baseline.rds')
glimpse(baseline)

# Read the studysheet_trialarms_updated.rds from processed folder to extract the sample size
study_trialarms <- readRDS('data/glp1ra/processed/studysheet_trialarms_updated.rds')
glimpse(study_trialarms)

######################################################################
# Left join, Baseline(master) with studysheet_trialarms by ARMID ----
#####################################################################
study_trialarms_baseline <- baseline |> 
  left_join(study_trialarms |> 
              select('ARMID','treatment_group','baseline_N_per_arm','age','treatment_arm_nameinitials',
                     'last_name','publication_year','female_count','female_pct'), by ='ARMID', relationship = "many-to-many")
glimpse(study_trialarms_baseline)

###########################################################################
# Fill in confidence intervals for study_trialarms_baseline ----
##########################################################################
study_trialarms_baseline_CIs_complete <- update_confidence_bounds(data = study_trialarms_baseline,
                                                                                     mean_col = 'baseline_mean',standard_deviation_col = 'baseline_sd',
                                                                                     standard_error_col = 'baseline_se',sample_size_col = 'baseline_N_per_arm',
                                                                                     lower_bound_col = 'baseline_lowerbound', 
                                                                                     upper_bound_col = 'baseline_upperbound',z=1.96,digits = 2)
glimpse(study_trialarms_baseline_CIs_complete)


###########################################
# Fill in the SEs and SDs----
##########################################

study_trialarms_baseline_CIsSDsSEs_complete <- calc_sd_se_from_ci(data = study_trialarms_baseline_CIs_complete , lower_col = baseline_lowerbound, upper_col = baseline_upperbound, 
                                          n_col = baseline_N_per_arm, ci_level = .95, se_col = baseline_se, sd_col = baseline_sd)
glimpse(study_trialarms_baseline_CIsSDsSEs_complete)


#####################################################################################
# Save the output into a clean rds file in processed folder ----
#####################################################################################
saveRDS(study_trialarms_baseline_CIsSDsSEs_complete, file = 'data/glp1ra/processed/Study_trialarms_baseline_CIsSDsSEs_updated.rds')
