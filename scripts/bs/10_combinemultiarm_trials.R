################################################################################
# Script for combining multiarms----
# Handle, SBP, DBP and BMI outcomes
################################################################################

# Load the libraries and functions
source('R/shared/setup.R')

# Read in the data
baseline_post <- readRDS('data/bs/processed/baseline_post_relabelmultiarms.rds')
glimpse(baseline_post)

# Filter to keep follow-up time points
baseline_post |> 
  filter(post_time_months != 0)
