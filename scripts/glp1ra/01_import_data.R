###################################################################################################
# Script for importing all my data from the excel sheet-----
# Data will be used by other scripts
###################################################################################################

########################################
# Load the libraries----
########################################
source('R/shared/setup.R')

# File paths
study_outcome <- 'data/glp1ra/processed/ExploratoryDataanalysis_GLP1RAs.xlsx'

file <- 'data/glp1ra/raw/DataExtractionGLP1RsWG.xlsm'

###########################################
# Read the sheets----
###########################################
study_outcome_map <- read_excel(study_outcome, sheet = 'StudyID_Followupmatrix')
study_outcome_map

studysheet <- read_excel(file, sheet = 'Studysheet', skip = 1) # skips first row which is a header
studysheet
studysheet <- studysheet |> 
  rename('last_name' = 'First author last name',
         'publication_year' = 'Publication year',
         'number_treatmentarms' = 'Number of comparisons',
         'studysheetNotes' = 'Notes'
         
  )

glimpse(studysheet)

trialarms <- read_excel(file, sheet = 'Trialarms', skip = 1) # skips first row which is a header
glimpse(trialarms)
trialarms <- trialarms |> 
  rename('baseline_N_per_arm' = 'SampleSize of treatment/comparator',
         'age' = 'Age of the participants per trial arm',
         'female_count' = 'Number of females',
         'female_pct' = 'Proportion (percent) of females',
         'treatment_arm_nameinitials' = 'TreatmentArmName',
         'comments'='...11',
         'treatment_group' = 'TrialArmType',
         'dosage' = 'Dosage for GLP1RAs',
         'dosage_units' = 'Dosage units',
         
         
  )
glimpse(trialarms)


baseline <- read_excel(file, sheet = 'Baselinesheet', skip = 1) # skips first row which is a header
glimpse(baseline)
baseline <- baseline |> 
  rename('baseline_mean' = 'Mean',
         'baseline_lowerbound' = 'lower_CI',
         'baseline_upperbound' = 'upper_CI',
         'baseline_sd' = 'SD',
         'baseline_se' = 'SE',
         'baseline_CI_level' = 'CI_level (%)',
         'units' = 'Units of measurement',
         'hyertension_estimate_freetext' = 'Note, free text describing hypertension trait such as number of hypertensives',
         'numberhypertensive_freetext' = 'Number of people using antihypertensives or were hypertensive @baseline',
         'prophypertensiveatbaseline' = 'Proportion of people using antihypertensives or were hypertensive @baseline',
         'Notes_estimatororsubgroup' = 'Notes fo any estimate or group',
         'ARMID' = 'ArmID'
  )
glimpse(baseline) # 327

######################################################################################################
# Clean up the baseline of non-continuous data----
# Drop rows with hypertension as outcome, baseline_mean=NA, Note column has 24 h ambulatory BP
#####################################################################################################
# Filter out Rows with Hypertension (n = 24) as outcome (filter or filter_out)----
baseline <- filter(baseline,Outcome != 'Hypertension')
glimpse(baseline) # 291 rows
# Filter out the mean_baseline is NA (n =4) (the authors reported, SDs and SEs only)
baseline <- filter(baseline,!is.na(baseline_mean))
glimpse(baseline) # 291 rows
# Filter out the Note column for string if entry starts with 24-h ambulatory (n = 8)
baseline <- filter(baseline,!grepl('24-h', Notes_estimatororsubgroup))  # 291 rows
glimpse(baseline) # 291 rows
# Filter out the Note column for string if entry starts with median (n=3)
baseline <- filter(baseline, !grepl('Median', Notes_estimatororsubgroup)) # N = 291
glimpse(baseline) # 291 rows
# Filter out the Note column for string if entry starts with interquartle range (n=3)
baseline <- filter(baseline, !grepl('interquartile', Notes_estimatororsubgroup)) # N = 291
glimpse(baseline) # 291 rows

# Results (actual means and change form baseline)
post <- read_excel(file, sheet = 'Resultssheet', skip = 1) # skips first row which is a header
glimpse(post)
# Rename some columns
post <- post |> 
  rename('ARMID' = 'ArmID', 'post_time_weeks' = 'Study duration; Time_Weeks', 'post_time_months' = 'Study duration; Time_months',
         'post_mean' = 'Mean', 'post_lowerbound' = 'lower_CI','post_upperbound' = 'upper_CI','post_sd' = 'SD', 'post_se' = 'SE',
         'post_CI_level' = 'CI_level (%)', 'post_p_value' = 'P-value', 'post_samplesize' = 'SampleSize', 'post_units' ='Units of measurement', 
         'post_htn' = 'Hypertension estimate', 'post_Notes' = 'Notes', 'post_data_source' = 'Data source (a table or figure number)')
glimpse(post) # 56 rows

# #####################################################################################################
# post results (actual means BMI and BP----
# Drop rows with hypertension as outcome, post_mean=NA, post_Notes column has 24 h ambulatory BP
#####################################################################################################
# Filter out Rows with Hypertension (n = 0) as outcome (filter or filter_out)----
post <- filter(post,Outcome != 'Hypertension')
glimpse(post) # 56 rows
# Filter out the post_mean is NA (the authors reported, SDs and SEs only)
post <- filter(post,!is.na(post_mean))
glimpse(post) # 56 rows
# Filter out the post_Notes column for string if entry starts with 24-h ambulatory (n = 0)
post <- filter(post,!grepl('24 h',post_Notes))  # 56 rows
glimpse(post)
# Filter out the post_Notes column for string if entry starts with 24-h ambulatory (n = 0)
post <- filter(post,!grepl('ambulatory', post_Notes))  # 56 rows
glimpse(post)

# Filter out the post_Notes column for string if entry starts with median (n = 0)
post <- filter(post, !grepl('Median', post_Notes)) # N = 56 rows
glimpse(post)
# Filter out the post_Notes column for string if entry starts with Interquartile range (n = 0)
post <- filter(post, !grepl('Interquartile', post_data_source)) # N = 56
glimpse(post)

# Results - change from baseline
change <- read_excel(file, sheet = 'ResultsChangefromBaseline', skip = 1) # skips first row which is a header
glimpse(change)
# Rename some columns
change <- change |> 
  rename('ARMID' = 'ArmID', 'change_time_weeks' = 'Study duration; Time_Weeks', 'change_time_months' = 'Study duration; Time_months',
         'change_mean' = 'meanChangefrombaseline', 'change_lowerbound' = 'lowerCIChangefrombaseline', 'change_upperbound' = 'upperCIChangefrombaseline',
         'change_sd' = 'SDchangefrombaseline', 'change_se' = 'SEofChangefrom Baseline', 'change_CI_level' = 'CI_level (%)', 'change_p_value' = 'P-value',
         'change_samplesize' = 'SampleSize', 'change_units' ='UnitofMeasurement', 'change_htn' = 'Hypertension estimate',  
         'change_data_source' = 'Data source (a table or figure number)', 'change_direction' = 'Directionofchangefrombaseline such as follow-up - baseline)',
         'change_hypertensives' = 'Note(for hypertension outcome such as reduced number of hypertensives)')
glimpse(change)
#####################################################################################################
# Change from baseline ----
# Drop rows with hypertension as outcome, change_mean=NA, change_Notes column has 24 h ambulatory BP
# and rows with Interquartile range
#####################################################################################################
# Filter out Rows with Hypertension (n = 2) as outcome (filter or filter_out)----
change <- filter(change,Outcome != 'Hypertension')
glimpse(change) # 358 rows
# Filter out the change_mean is NA (the authors reported, SDs and SEs only)
change <- filter(change,!is.na(change_mean))
glimpse(change) # 358 rows
# Filter out the Note column for string if entry starts with 24-h ambulatory (n = 4)
change <- filter(change,!grepl('24 h', change_data_source))  # 358 rows
glimpse(change)
# Filter out the Note column for string if entry starts with 24-h ambulatory (n = 0)
change <- filter(change,!grepl('ambulatory', change_data_source))  # 358 rows
glimpse(change)
# Filter out the post_Notes column for string if entry starts with Interquartile range (n = 0)
change <- filter(change, !grepl('Interquartile', change_data_source)) # N = 358
glimpse(change) # 358 rows
################################################
# Save the data to be used by other scripts----
###############################################
saveRDS(study_outcome_map, file = 'data/glp1ra/processed/glp1ra_study_outcome_map.rds')
saveRDS(studysheet, file = 'data/glp1ra/processed/studysheet.rds')
saveRDS(trialarms, file = 'data/glp1ra/processed/trialarms.rds')
saveRDS(baseline, file = 'data/glp1ra/processed/baseline.rds')
saveRDS(post, file = 'data/glp1ra/processed/post.rds')
saveRDS(change, file = 'data/glp1ra/processed/change.rds')
