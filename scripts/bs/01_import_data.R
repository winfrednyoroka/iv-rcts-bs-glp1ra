###################################################################################################
# Script for importing all my data from the excel sheet-----
# Data will be used by other scripts
###################################################################################################

########################################
# Load the libraries----
########################################
source('R/shared/setup.R')

# File paths
study_outcome <- 'data/bs/processed/ExploratoryDataAnalysis.xlsx'

file <- 'data/bs/raw/DataExtractionBSWG.xlsm'

###########################################
# Read the sheets----
# Update the column names where needed
###########################################
study_outcome_map <- read_excel(study_outcome, sheet = 'StudyID_Followupmatrix')
study_outcome_map

studysheet <- read_excel(file, sheet = 'Studysheet', skip = 1) # skips first row which is a header
glimpse(studysheet)
studysheet <- studysheet |> 
  rename('last_name' = 'First author last name',
         'publication_year' = 'Publication year',
         'number_treatmentarms' = 'Number of comparisons',
         'studysheetNotes' = 'Notes'
         
  )

glimpse(studysheet)

trialarms <- read_excel(file, sheet = 'TrialArms', skip = 1) # skips first row which is a header
glimpse(trialarms)
trialarms <- trialarms |> 
  rename('baseline_N_per_arm' = 'SampleSize of treatment/comparator',
         'age' = 'Age of the participants per trial arm',
         'female_count' = 'Number of females',
         'female_pct' = 'Proportion (percent) of females',
         'treatment_arm_nameinitials' = 'TreatmentArmName',
         'treatment_arm_fullname'='Notes',
         'treatment_group' = 'TrialArmType',
         'dosage' = 'Dosage for GLP1RAs',
         'dosage_units' = 'Dosage units',
         
         
  )
glimpse(trialarms)

baseline <- read_excel(file, sheet = 'Baselinesheet', skip = 1) # skips first row which is a header
glimpse(baseline)
# Rename some column names
baseline <- baseline |> 
  rename('baseline_mean' = 'Mean',
         'baseline_lowerbound' = 'lower_CI',
         'baseline_upperbound' = 'upper_CI',
         'baseline_sd' = 'SD',
         'baseline_se' = 'SE',
         'baseline_CI_level' = 'CI_level (%)',
         'units' = 'Units of measurement',
         'hyertension_estimate_freetext' = 'Note, free text describing hypertension trait such as number of hypertensives',
         'numberhypertensive_freetext' = 'Number of people using antihypertensives @baseline',
         'prophypertensiveatbaseline' = 'Proportion of people using antihypertensives @baseline',
         'ARMID' = 'ArmID'
  )
glimpse(baseline) # 184 rows

######################################################################################################
# Clean up the baseline of non-continuous data----
# Drop rows with hypertension as outcome, baseline_mean=NA, Note column has 24 h ambulatory BP
#####################################################################################################
# Filter out Rows with Hypertension (n = 24) as outcome (filter or filter_out)----
baseline <- filter(baseline,Outcome != 'Hypertension')
glimpse(baseline) # 160 rows
# Filter out the mean_baseline is NA (n =4) (the authors reported, SDs and SEs only)
baseline <- filter(baseline,!is.na(baseline_mean))
glimpse(baseline) # 160 rows
# Filter out the Note column for string if entry starts with 24-h ambulatory (n = 8)
baseline <- filter(baseline,!grepl('24-h', Note))  # 156 rows
glimpse(baseline) # 152 rows
# Filter out the Note column for string if entry starts with median (n=3)
baseline <- filter(baseline, !grepl('Median', Note)) # N = 153
glimpse(baseline) # 149 rows
# Filter out the Note column for string if entry starts with interquartle range (n=3)
baseline <- filter(baseline, !grepl('interquartile', Note)) # N = 143
glimpse(baseline) # 143 rows

# Results (actual means and change form baseline)
post <- read_excel(file, sheet = 'Resultssheet', skip = 1) # skips first row which is a header
glimpse(post)
# Rename some columns
post <- post |> 
  rename('ARMID' = 'ArmID', 'post_time_weeks' = 'Study duration; Time_weeks', 'post_time_months' = 'Study duration; Time_months',
         'post_mean' = 'Mean', 'post_lowerbound' = 'lower_CI','post_upperbound' = 'upper_CI','post_sd' = 'SD', 'post_se' = 'SE',
         'post_CI_level' = 'CI_level (%)', 'post_p_value' = 'P-value', 'post_samplesize' = 'SampleSize', 'post_units' ='Units of measurement', 
         'post_htn' = 'Hypertension estimate', 'post_Notes' = 'Notes', 'post_data_source' = 'Data source (a table or figure number)')
glimpse(post) # 224 rows

# #####################################################################################################
# post results (actual means BMI and BP----
# Drop rows with hypertension as outcome, post_mean=NA, post_Notes column has 24 h ambulatory BP
#####################################################################################################
# Filter out Rows with Hypertension (n = 25) as outcome (filter or filter_out)----
post <- filter(post,Outcome != 'Hypertension')
glimpse(post) # 199 rows
# Filter out the post_mean is NA (the authors reported, SDs and SEs only)
post <- filter(post,!is.na(post_mean))
glimpse(post) # 199 rows
# Filter out the post_Notes column for string if entry starts with 24-h ambulatory (n = 0)
post <- filter(post,!grepl('24 h',post_Notes))  # 199 rows
glimpse(post)
# Filter out the post_Notes column for string if entry starts with 24-h ambulatory (n = 0)
post <- filter(post,!grepl('ambulatory', post_Notes))  # 187 rows
glimpse(post)

# Filter out the post_Notes column for string if entry starts with median (n = 0)
post <- filter(post, !grepl('Median', post_Notes)) # N = 187
glimpse(post)
# Filter out the post_Notes column for string if entry starts with Interquartile range (n = 0)
post <- filter(post, !grepl('Interquartile', post_data_source)) # N = 179
glimpse(post)

# Results reported as change form baseline
change <- read_excel(file, sheet = 'ResultsChangefromBaseline', skip = 1) # skips first row which is a header
glimpse(change)

# Rename some columns
change <- change |> 
  rename('ARMID' = 'ArmID', 'change_time_weeks' = 'Study duration; Time_weeks', 'change_time_months' = 'Study duration; Time_months',
         'change_mean' = 'meanChangefrombaseline', 'change_lowerbound' = 'lowerCIChangefrombaseline', 'change_upperbound' = 'upperCIChangefrombaseline',
         'change_sd' = 'SDchangefrombaseline', 'change_se' = 'SEofChangefrombaseline', 'change_CI_level' = 'CI_level (%)', 'change_p_value' = 'P-value',
         'change_samplesize' = 'SampleSize', 'change_units' ='UnitofMeasurement', 'change_htn' = 'Hypertension estimate', 'change_Notes' = 'Note', 
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
glimpse(change) # 116 rows
# Filter out the change_mean is NA (the authors reported, SDs and SEs only)
change <- filter(change,!is.na(change_mean))
glimpse(change) # 116 rows
# Filter out the Note column for string if entry starts with 24-h ambulatory (n = 4)
change <- filter(change,!grepl('24 h',change_Notes))  # 112 rows
glimpse(change)
# Filter out the Note column for string if entry starts with 24-h ambulatory (n = 0)
change <- filter(change,!grepl('ambulatory',change_Notes))  # 112 rows
glimpse(change)
# Filter out the post_Notes column for string if entry starts with Interquartile range (n = 0)
change <- filter(change, !grepl('Interquartile', change_data_source)) # N = 108
glimpse(change) # 108 rows
################################################
# Save the data to be used by other scripts----
###############################################
saveRDS(study_outcome_map, file = 'data/bs/processed/bs_study_outcome_map.rds')
saveRDS(studysheet, file = 'data/bs/processed/studysheet.rds')
saveRDS(trialarms, file = 'data/bs/processed/trialarms.rds')
saveRDS(baseline, file = 'data/bs/processed/baseline.rds')
saveRDS(post, file = 'data/bs/processed/post.rds')
saveRDS(change, file = 'data/bs/processed/change.rds')
