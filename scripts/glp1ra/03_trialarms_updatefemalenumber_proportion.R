#########################################################
#### IV Analysis Applied to Bariatric Surgrey RCTs ######
#########################################################
## The main aim of the study is:
## What is the effect of BMI difference induced by bariatric surgery on BP using
## Using IV analysis framework
## Randomisation as an IV, instrumenting weight loss

##########################################################################################
# Script for updating female number and proportion (expressed as pct e.g 60% and not 0.6)
# per arm using sample size and either count or proportion, sample size can not be zero
# Writes the updated data in a labelled rds file in the data/processed folder
#########################################################################################

##########################################
# Load libraries and custom functions----
#########################################
source('R/shared/setup.R')
source('R/shared/update_female_number_percentage.R')

################
# Read data----
###############
# Read in Studysheet - extract author name and publication year
studysheet <- readRDS('data/glp1ra/processed/studysheet.rds')
glimpse(studysheet)

# Read the TrialArms sheet
trialarms <- readRDS('data/glp1ra/processed/trialarms.rds')
glimpse(trialarms)

#############################################################################################
# Left join the studysheet into trialarms----
# Match the data last_name and publication_year from studysheet to trialarms using ID column
# The data will be duplicated since the RCTs have 2 or more treatment arms
############################################################################################
studysheet_trialarms <- trialarms |> 
  left_join(studysheet |> 
              select('ID','last_name','publication_year'), by = 'ID')
glimpse(studysheet_trialarms)

#####################################################################################
# Update the female count and proportion using the custom fill_female_data function----
#####################################################################################
studysheet_trialarms |> 
  summarise(
    female_n_na = sum(is.na(female_count)),
    female_prop_na = sum(is.na(female_pct))
  ) # 17, 8 nas

studysheet_trialarms <- fill_female_data(studysheet_trialarms, sample_size_col = 'baseline_N_per_arm',
                                         female_n_col = 'female_count',female_prop_col = 'female_pct')
glimpse(studysheet_trialarms)
#####################################################################################
# Check for nas in the female_count and female_pct after function call----
# The output of the two columns must be zero
#####################################################################################
studysheet_trialarms |> 
  summarise(
    female_n_na = sum(is.na(female_count)),
    female_prop_na = sum(is.na(female_pct))
  ) # 0, 0 nas

#####################################################################################
# Save the output into an rds object processed folder----
#####################################################################################
saveRDS(studysheet_trialarms, file = 'data/glp1ra/processed/studysheet_trialarms_updated.rds')
