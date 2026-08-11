################################################################################
# Script for performing meta-analysis where possible using metafor----
# Visualise the results of meta-analysis using forest plot
################################################################################

# Load the libraries and functions when necessary
source('R/shared/setup.R')
source('R/shared/run_metaanalysis.R')
source('R/shared/forest_plot_metafor.R')

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

# Meta-analysis ----
# Temporality
fit_bmi12_sbp24 <- run_rem_meta(bmi12_sbp24)
summary(fit_bmi12_sbp24)
fit_bmi12_dbp24 <- run_rem_meta(bmi12_dbp24)
summary(fit_bmi12_dbp24)
# @12 months----
fit_bmi12_sbp12 <- run_rem_meta(bmi12_sbp12)
summary(fit_bmi12_sbp12)
fit_bmi12_dbp12 <- run_rem_meta(bmi12_dbp12)
summary(fit_bmi12_dbp12)
# @24 months----
fit_bmi24_sbp24 <- run_rem_meta(bmi24_sbp24)
summary(fit_bmi24_sbp24)
fit_bmi24_dbp24 <- run_rem_meta(bmi24_dbp24)
summary(fit_bmi24_dbp24)

# Visualise ----
plot_forest(fit_bmi12_sbp24, xlab ='Effect of BMI change at 12 months on SBP change at 24 months')
plot_forest(fit_bmi12_dbp24, xlab ='Effect of BMI change at 12 months on DBP change at 24 months')
plot_forest(fit_bmi12_sbp12, xlab ='Effect of BMI change at 12 months on SBP change at 12 months')
plot_forest(fit_bmi12_dbp12, xlab ='Effect of BMI change at 12 months on DBP change at 12 months')
plot_forest(fit_bmi24_sbp24, xlab ='Effect of BMI change at 24 months on SBP change at 24 months')
plot_forest(fit_bmi24_dbp24, xlab ='Effect of BMI change at 24 months on DBP change at 24 months')
