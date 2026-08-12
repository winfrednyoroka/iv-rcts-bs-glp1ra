######################################################################
# Script for hypothesis testing using two sample t-test----
# Did the treatment groups start with the same mean BMI, DBP and SBP?
#####################################################################
# Load libraries and packages
source('R/shared/setup.R')
source('R/shared/t_testforhypothesistesting.R')

# Read in  the data
baseline_post <- readRDS('data/glp1ra/processed/baseline_post_relabelmultiarms.rds')
glimpse(baseline_post)

##################################################
# Filter to keep time point 0 only ----
# Pivot the data intervention vs control
# two sample independent t-test
##################################################
baseline_tests <- baseline_post |> 
  filter(post_time_months == 0) |> 
  select(
    comparison_id,
    Outcome,
    treatment_group,
    mean,
    sd,
    samplesize
  ) |> 
  pivot_wider(
    names_from = treatment_group,
    values_from = c(mean, sd, samplesize)
  ) 

baseline_tests


#########################
# BMI t-test ----
#########################
bmi_t_test <- baseline_tests |> 
  filter(Outcome == 'BMI') |> 
compare_baseline(
                 mean_int = 'mean_Intervention',
                 mean_con = 'mean_Control',
                 sd_int = 'sd_Intervention',
                 sd_con = 'sd_Control',
                 n_int = 'samplesize_Intervention',
                 n_con = 'samplesize_Control',
                 label_col = "comparison_id")
bmi_t_test
###############################
# Save the bmi_t-test results-----
##############################
write_csv(bmi_t_test, 'output/glp1ra/tables/bmi_t-test_results.csv')


#########################
# SBP t-test ----
#########################
sbp_t_test <- baseline_tests |> 
  filter(Outcome == 'SBP') |> 
  compare_baseline(
    mean_int = 'mean_Intervention',
    mean_con = 'mean_Control',
    sd_int = 'sd_Intervention',
    sd_con = 'sd_Control',
    n_int = 'samplesize_Intervention',
    n_con = 'samplesize_Control',
    label_col = "comparison_id")
sbp_t_test
###############################
# Save the sbp_t-test results-----
##############################
write_csv(sbp_t_test, 'output/glp1ra/tables/sbp_t-test_results.csv')

#########################
# DBP t-test ----
#########################
dbp_t_test <- baseline_tests |> 
  filter(Outcome == 'DBP') |> 
  compare_baseline(
    mean_int = 'mean_Intervention',
    mean_con = 'mean_Control',
    sd_int = 'sd_Intervention',
    sd_con = 'sd_Control',
    n_int = 'samplesize_Intervention',
    n_con = 'samplesize_Control',
    label_col = "comparison_id")
dbp_t_test
###############################
# Save the sbp_t-test results-----
##############################
write_csv(dbp_t_test, 'output/glp1ra/tables/dbp_t-test_results.csv')

