################################################################################
# Script for performing meta-analysis where possible using metafor----
# Visualise the results of meta-analysis using forest plot
################################################################################

# Load the libraries and functions when necessary
source('R/shared/setup.R')

# Read in the data
# Temporality (bmi @12 months and bp @24 months)----
bmi12_sbp24 <- readRDS('data/bs/processed/wr_bmi12_sbp24.rds')
bmi12_sbp24

bmi12_dbp24 <- readRDS('data/bs/processed/wr_bmi12_dbp24.rds')
bmi12_dbp24
# bmi and bp @12 months----
bmi12_sbp12 <- readRDS('data/bs/processed/wr_bmi12_sbp12.rds')
bmi12_sbp12

bmi12_dbp12 <- readRDS('data/bs/processed/wr_bmi12_dbp12.rds')
bmi12_dbp12

# bmi and bp @24 months----
bmi24_sbp24 <- readRDS('data/bs/processed/wr_bmi24_sbp24.rds')
bmi24_sbp24

bmi24_dbp24 <- readRDS('data/bs/processed/wr_bmi24_dbp24.rds')
bmi24_dbp24
