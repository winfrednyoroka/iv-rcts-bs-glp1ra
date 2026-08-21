################################################################################
# Script calculating IV estimates
# Calculate the effects i.e betas to be used for Wald ratio
################################################################################
# Load libraries and functions
source('R/shared/setup.R')
source('R/glp1ra/calc_wr_effectsformultipletimepoints.R')
source('R/shared/wald_ratioestimator.R')


# Read in the data
base_post <- readRDS('data/glp1ra/processed/baseline_post_relabelmultiarms.rds')
glimpse(base_post)

# Filter to keep only the non baseline
post <- base_post |> 
  filter(post_time_months != 0) # remove all baseline measures
glimpse(post)
head(post, n=20)
# Drop Wharton (2023)
post <- post |> 
  filter( !grepl("Wharton", comparison_id))
post
post |>
  count(
    comparison_id,
    Outcome,
    post_time_months,
    treatment_group
  ) |>
  filter(n > 1)
# # Create treatment effects first
effects <- post |>
  select(
    comparison_id,
    Outcome,
    post_time_months,
    treatment_group,
    mean,
    sd,
    samplesize
  ) |>
  pivot_wider(
    id_cols = c(
      comparison_id,
      Outcome,
      post_time_months
    ),
    names_from = treatment_group,
    values_from = c(mean, sd, samplesize)
  ) |> 
  mutate(
    beta = mean_Intervention - mean_Control,
    
    se = sqrt(
      (sd_Intervention^2 / samplesize_Intervention) +
        (sd_Control^2 / samplesize_Control)
    )
  ) |>
  select(
    comparison_id,
    Outcome,
    post_time_months,
    beta,
    se
  )
glimpse(effects)


########################################################################
# IV estimate BMI and BP (SBP,DBP) at 6, 12, 16, 17 and 24 months----
#######################################################################
# BMI and SBP @6, 12, 16, 17 and 24 months----
wr_bmi_dbp <- calc_wr(
  data = effects,
  exposure_outcome = "BMI",
  outcome_outcome = "DBP",
  times = c(6, 12, 16, 17, 24)
)
wr_bmi_dbp

wr_bmi_sbp <- calc_wr(
  data = effects,
  exposure_outcome = "BMI",
  outcome_outcome = "SBP",
  times = c(6, 12, 16, 17, 24)
)
wr_bmi_sbp

# BMI and SBP at 6 months
wr_bmi6_sbp6 <- wr_bmi_sbp |>  wald_ratio(
  beta_y = "beta_SBP_6",
  se_y   = "se_SBP_6",
  beta_x = "beta_BMI_6",
  se_x   = "se_BMI_6"
)
wr_bmi6_sbp6 

# Drop rows wiht NAs
wr_bmi6_sbp6_clean <- wr_bmi6_sbp6 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  ) |> 
  select(comparison_id, beta_BMI_6, beta_SBP_6, WR, WR_SE,WR_Lower95CI,WR_Upper95CI)
wr_bmi6_sbp6_clean

# Save rds object and csv----
saveRDS(wr_bmi6_sbp6_clean, 'data/glp1ra/processed/wr_bmi6_sbp6.rds')
write_csv(wr_bmi6_sbp6_clean, 'output/glp1ra/tables/wr_bmi6_sbp6.csv')

# # BMI @6 months and DBP @6 months----
wr_bmi6_dbp6 <- wr_bmi_dbp |>  wald_ratio(
  beta_y = "beta_DBP_6",
  se_y   = "se_DBP_6",
  beta_x = "beta_BMI_6",
  se_x   = "se_BMI_6"
)
wr_bmi6_dbp6

# Drop rows wiht NAs
wr_bmi6_dbp6_clean <- wr_bmi6_dbp6 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  ) |> 
  select(comparison_id, beta_BMI_6, beta_DBP_6, WR, WR_SE, WR_Lower95CI, WR_Upper95CI)
wr_bmi6_dbp6_clean

# Save rds object and csv----
saveRDS(wr_bmi6_dbp6_clean, 'data/glp1ra/processed/wr_bmi6_dbp6.rds')
write_csv(wr_bmi6_dbp6_clean, 'output/glp1ra/tables/wr_bmi6_dbp6.csv')

# BMI and SBP @16 months----
wr_bmi16_sbp16 <- wr_bmi_sbp |>  wald_ratio(
  beta_y = "beta_SBP_16",
  se_y   = "se_SBP_16",
  beta_x = "beta_BMI_16",
  se_x   = "se_BMI_16"
)
wr_bmi16_sbp16

# Drop rows wiht NAs
wr_bmi16_sbp16_clean <- wr_bmi16_sbp16 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  ) |> 
  select(comparison_id, beta_BMI_16, beta_SBP_16, WR, WR_SE, WR_Lower95CI, WR_Upper95CI)
wr_bmi16_sbp16_clean

# Save rds object and csv----
saveRDS(wr_bmi16_sbp16_clean, 'data/glp1ra/processed/wr_bmi16_sbp16.rds')
write_csv(wr_bmi16_sbp16_clean, 'output/glp1ra/tables/wr_bmi16_sbp16.csv')

# BMI and DBP @16 months----
wr_bmi16_dbp16 <- wr_bmi_dbp |>  wald_ratio(
  beta_y = "beta_DBP_16",
  se_y   = "se_DBP_16",
  beta_x = "beta_BMI_16",
  se_x   = "se_BMI_16"
)

# Drop rows with NAs
wr_bmi16_dbp16_clean <- wr_bmi16_dbp16 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  ) |> 
  select(comparison_id, beta_BMI_16, beta_DBP_16, WR, WR_SE, WR_Lower95CI, WR_Upper95CI)
wr_bmi16_dbp16_clean

# Save rds object and csv----
saveRDS(wr_bmi16_dbp16_clean, 'data/glp1ra/processed/wr_bmi16_dbp16.rds')
write_csv(wr_bmi16_dbp16_clean, 'output/glp1ra/tables/wr_bmi16_dbp16.csv')

# BMI and SBP @17 months----
wr_bmi17_sbp17 <- wr_bmi_sbp |>  wald_ratio(
  beta_y = "beta_SBP_17",
  se_y   = "se_SBP_17",
  beta_x = "beta_BMI_17",
  se_x   = "se_BMI_17"
)
wr_bmi17_sbp17

# Drop rows wiht NAs
wr_bmi17_sbp17_clean <- wr_bmi17_sbp17 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  ) |> 
  select(comparison_id, beta_BMI_17, beta_SBP_17, WR, WR_SE, WR_Lower95CI, WR_Upper95CI)
wr_bmi17_sbp17_clean

# Save rds object and csv----
saveRDS(wr_bmi17_sbp17_clean, 'data/glp1ra/processed/wr_bmi17_sbp17.rds')
write_csv(wr_bmi17_sbp17_clean, 'output/glp1ra/tables/wr_bmi17_sbp17.csv')

# BMI and DBP @17 months----
wr_bmi17_dbp17 <- wr_bmi_dbp |>  wald_ratio(
  beta_y = "beta_DBP_17",
  se_y   = "se_DBP_17",
  beta_x = "beta_BMI_17",
  se_x   = "se_BMI_17"
)
wr_bmi17_dbp17

# Drop rows with NAs
wr_bmi17_dbp17_clean <- wr_bmi17_dbp17 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  ) |> 
  select(comparison_id, beta_BMI_17, beta_DBP_17, WR, WR_SE, WR_Lower95CI, WR_Upper95CI)
wr_bmi17_dbp17_clean

# Save rds object and csv----
saveRDS(wr_bmi17_dbp17_clean, 'data/glp1ra/processed/wr_bmi17_dbp17.rds')
write_csv(wr_bmi17_dbp17_clean, 'output/glp1ra/tables/wr_bmi17_dbp17.csv')

# BMI and SBP @24 months----
wr_bmi24_sbp24 <- wr_bmi_sbp |>  wald_ratio(
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
  ) |> 
  select(comparison_id, beta_BMI_24, beta_SBP_24, WR, WR_SE, WR_Lower95CI, WR_Upper95CI)
wr_bmi24_sbp24_clean

# Save rds object and csv----
saveRDS(wr_bmi24_sbp24_clean, 'data/glp1ra/processed/wr_bmi24_sbp24.rds')
write_csv(wr_bmi24_sbp24_clean, 'output/glp1ra/tables/wr_bmi24_sbp24.csv')
# BMI and DBP @24 months----
wr_bmi24_dbp24 <- wr_bmi_dbp |>  wald_ratio(
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
  ) |> 
  select(comparison_id, beta_BMI_24, beta_DBP_24, WR, WR_SE, WR_Lower95CI, WR_Upper95CI)
wr_bmi24_dbp24_clean

# Save rds object and csv----
saveRDS(wr_bmi24_dbp24_clean, 'data/glp1ra/processed/wr_bmi24_dbp24.rds')
write_csv(wr_bmi24_dbp24_clean, 'output/glp1ra/tables/wr_bmi24_dbp24.csv')

