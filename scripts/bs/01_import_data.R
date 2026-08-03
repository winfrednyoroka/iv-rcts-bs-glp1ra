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

baseline <- read_excel(file,sheet = 'Baselinesheet', skip = 1) # skips first row which is a header
baseline

# Results (actual means and change form baseline)
post <- read_excel(file, sheet = 'Resultssheet', skip = 1) # skips first row which is a header
post

change <- read_excel(file, sheet = 'ResultsChangefromBaseline', skip = 1) # skips first row which is a header
change
################################################
# Save the data to be used by other scripts----
###############################################
saveRDS(study_outcome_map, file = 'data/bs/processed/bs_study_outcome_map.rds')
saveRDS(studysheet, file = 'data/bs/processed/studysheet.rds')
saveRDS(trialarms, file = 'data/bs/processed/trialarms.rds')
saveRDS(baseline, file = 'data/bs/processed/baseline.rds')
saveRDS(post, file = 'data/bs/processed/post.rds')
saveRDS(change, file = 'data/bs/processed/change.rds')
