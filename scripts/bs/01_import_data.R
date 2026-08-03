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
###########################################
study_outcome_map <- read_excel(study_outcome, sheet = 'StudyID_Followupmatrix')
study_outcome_map

studysheet <- read_excel(file, sheet = 'Studysheet', skip = 1) # skips first row which is a header
studysheet

trialarms <- read_excel(file, sheet = 'TrialArms', skip = 1) # skips first row which is a header
trialarms

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
