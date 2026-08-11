################################################################################
# Script calculating IV estimates
# Calculate the effects i.e betas to be used for Wald ratio
################################################################################
# Load libraries and functions
source('R/shared/setup.R')
source('R/shared/calc_wr_beta_effects.R')
source('R/shared/wald_ratioestimator.R')

# Read in the data
base_post_combined <- readRDS('data/bs/processed/baseline_post_combinedmultiarms.rds')
glimpse(base_post_combined)

# Filter to keep only the non baseline
post <- base_post_combined |> 
  filter(post_time_months != 0) # remove all baseline measures
glimpse(post)
# Create treatment effects first
effects <- post |> 
  select(
    study_id,
    Outcome,
    post_time_months,
    treatment_group,
    mean,
    sd,
    samplesize
  ) |> 
  pivot_wider(
    id_cols = c(
      study_id,
      Outcome,
      post_time_months
    ),
    names_from = treatment_group,
    values_from = c(
      mean,
      sd,
      samplesize
    )
  ) |> 
  mutate(
    beta = mean_Intervention - mean_Control,
    
    se = sqrt(
      (sd_Intervention^2 / samplesize_Intervention) +
        (sd_Control^2 / samplesize_Control)
    )
  ) |> 
  select(
    study_id,
    Outcome,
    post_time_months,
    beta,
    se
  )
glimpse(effects)

#################################################################
# IV estimate BMI at 12 months and BP (SBP,DBP) at 24 months
################################################################
# BMI @12 months and SBP @24 months----
wr_bmi12_sbp24 <- calc_wr(data = effects,
                          exposure_outcome = 'BMI',
                          exposure_time = 12,
                          outcome_outcome = 'SBP',
                          outcome_time = 24)
wr_bmi12_sbp24 <- wr_bmi12_sbp24 |>  wald_ratio(
  beta_y = "beta_SBP_24",
  se_y   = "se_SBP_24",
  beta_x = "beta_BMI_12",
  se_x   = "se_BMI_12"
)
wr_bmi12_sbp24

# Drop rows wiht NAs
wr_bmi12_sbp24_clean <- wr_bmi12_sbp24 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  )
wr_bmi12_sbp24_clean

# Save rds object and csv----
saveRDS(wr_bmi12_sbp24_clean, 'data/bs/processed/wr_bmi12_sbp24.rds')
write_csv(wr_bmi12_sbp24_clean, 'output/bs/tables/wr_bmi12_sbp24.csv')

# # BMI @12 months and DBP @24 months----
wr_bmi12_dbp24 <- calc_wr(data = effects,
                          exposure_outcome = 'BMI',
                          exposure_time = 12,
                          outcome_outcome = 'DBP',
                          outcome_time = 24)
wr_bmi12_dbp24 <- wr_bmi12_dbp24 |>  wald_ratio(
  beta_y = "beta_DBP_24",
  se_y   = "se_DBP_24",
  beta_x = "beta_BMI_12",
  se_x   = "se_BMI_12"
)
wr_bmi12_dbp24

# Drop rows wiht NAs
wr_bmi12_dbp24_clean <- wr_bmi12_dbp24 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  )
wr_bmi12_dbp24_clean

# Save rds object and csv----
saveRDS(wr_bmi12_dbp24_clean, 'data/bs/processed/wr_bmi12_dbp24.rds')
write_csv(wr_bmi12_dbp24_clean, 'output/bs/tables/wr_bmi12_dbp24.csv')

# BMI and SBP @12 months----
wr_bmi12_sbp12 <- calc_wr(data = effects,
                          exposure_outcome = 'BMI',
                          exposure_time = 12,
                          outcome_outcome = 'SBP',
                          outcome_time = 12)
wr_bmi12_sbp12 <- wr_bmi12_sbp12 |>  wald_ratio(
  beta_y = "beta_SBP_12",
  se_y   = "se_SBP_12",
  beta_x = "beta_BMI_12",
  se_x   = "se_BMI_12"
)
wr_bmi12_sbp12

# Drop rows wiht NAs
wr_bmi12_sbp12_clean <- wr_bmi12_sbp12 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  )
wr_bmi12_sbp12_clean

# Save rds object and csv----
saveRDS(wr_bmi12_sbp12_clean, 'data/bs/processed/wr_bmi12_sbp12.rds')
write_csv(wr_bmi12_sbp12_clean, 'output/bs/tables/wr_bmi12_sbp12.csv')

# BMI and DBP @12 months----
wr_bmi12_dbp12 <- calc_wr(data = effects,
                          exposure_outcome = 'BMI',
                          exposure_time = 12,
                          outcome_outcome = 'DBP',
                          outcome_time = 12)
wr_bmi12_dbp12 <- wr_bmi12_dbp12 |>  wald_ratio(
  beta_y = "beta_DBP_12",
  se_y   = "se_DBP_12",
  beta_x = "beta_BMI_12",
  se_x   = "se_BMI_12"
)
wr_bmi12_dbp12

# Drop rows with NAs
wr_bmi12_dbp12_clean <- wr_bmi12_dbp12 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  )
wr_bmi12_dbp12_clean

# Save rds object and csv----
saveRDS(wr_bmi12_dbp12_clean, 'data/bs/processed/wr_bmi12_dbp12.rds')
write_csv(wr_bmi12_dbp12_clean, 'output/bs/tables/wr_bmi12_dbp12.csv')

# BMI and SBP @24 months----
wr_bmi24_sbp24 <- calc_wr(data = effects,
                          exposure_outcome = 'BMI',
                          exposure_time = 24,
                          outcome_outcome = 'SBP',
                          outcome_time = 24)
wr_bmi24_sbp24 <- wr_bmi24_sbp24 |>  wald_ratio(
  beta_y = "beta_SBP_24",
  se_y   = "se_SBP_24",
  beta_x = "beta_BMI_24",
  se_x   = "se_BMI_24"
)
wr_bmi24_sbp24
# Drop rows with NAs
wr_bmi24_sbp24_clean <- wr_bmi24_sbp24 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  )
wr_bmi24_sbp24_clean

# Save rds object and csv----
saveRDS(wr_bmi24_sbp24_clean, 'data/bs/processed/wr_bmi24_sbp24.rds')
write_csv(wr_bmi24_sbp24_clean, 'output/bs/tables/wr_bmi24_sbp24.csv')
# BMI and DBP @24 months----
wr_bmi24_dbp24 <- calc_wr(data = effects,
                          exposure_outcome = 'BMI',
                          exposure_time = 24,
                          outcome_outcome = 'DBP',
                          outcome_time = 24)
wr_bmi24_dbp24 <- wr_bmi24_dbp24 |>  wald_ratio(
  beta_y = "beta_DBP_24",
  se_y   = "se_DBP_24",
  beta_x = "beta_BMI_24",
  se_x   = "se_BMI_24"
)
wr_bmi24_dbp24

# Drop rows wiht NAs
wr_bmi24_dbp24_clean <- wr_bmi24_dbp24 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  )
wr_bmi24_dbp24_clean

# Save rds object and csv----
saveRDS(wr_bmi24_dbp24_clean, 'data/bs/processed/wr_bmi24_dbp24.rds')
write_csv(wr_bmi24_dbp24_clean, 'output/bs/tables/wr_bmi24_dbp24.csv')

