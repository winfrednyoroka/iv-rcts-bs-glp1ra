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
glimpse(baseline)

# Results (actual means and change form baseline)
post <- read_excel(file, sheet = 'Resultssheet', skip = 1) # skips first row which is a header
post

change <- read_excel(file, sheet = 'ResultsChangefromBaseline', skip = 1) # skips first row which is a header
change
################################################
# Save the data to be used by other scripts----
###############################################
saveRDS(study_outcome_map, file = 'data/glp1ra/processed/glp1ra_study_outcome_map.rds')
saveRDS(studysheet, file = 'data/glp1ra/processed/studysheet.rds')
saveRDS(trialarms, file = 'data/glp1ra/processed/trialarms.rds')
saveRDS(baseline, file = 'data/glp1ra/processed/baseline.rds')
saveRDS(post, file = 'data/glp1ra/processed/post.rds')
saveRDS(change, file = 'data/glp1ra/processed/change.rds')
