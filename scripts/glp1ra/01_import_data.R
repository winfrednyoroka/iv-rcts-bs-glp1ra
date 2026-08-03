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

trialarms <- read_excel(file, sheet = 'Trialarms', skip = 1) # skips first row which is a header
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
saveRDS(study_outcome_map, file = 'data/glp1ra/processed/glp1ra_study_outcome_map.rds')
saveRDS(studysheet, file = 'data/glp1ra/processed/studysheet.rds')
saveRDS(trialarms, file = 'data/glp1ra/processed/trialarms.rds')
saveRDS(baseline, file = 'data/glp1ra/processed/baseline.rds')
saveRDS(post, file = 'data/glp1ra/processed/post.rds')
saveRDS(change, file = 'data/glp1ra/processed/change.rds')
