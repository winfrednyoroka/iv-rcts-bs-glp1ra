################################################################################
# Script calculating IV estimates
# Calculate the effects i.e betas to be used for Wald ratio
################################################################################
# Load libraries and functions
source('R/shared/setup.R')
source('R/shared/calc_wr_beta_effects.R')
source('R/shared/wald_ratioestimator.R')

# Read in the data
base_post <- readRDS('data/glp1ra/processed/baseline_post_relabelmultiarms.rds')
glimpse(base_post)

# Filter to keep only the non baseline
post <- base_post |> 
  filter(post_time_months != 0) # remove all baseline measures
glimpse(post)

# Align the data for analysis and visualisation----
controls <-
  post |>
  filter(treatment_group == "Control")

interventions <-
  post|>
  filter(treatment_group == "Intervention")
controls
interventions

# Left join the two datasets
fdat <-
  interventions |>
  left_join(
    controls,
    by = c("comparison_id", "post_time_months",'Outcome'),
    suffix = c("_int", "_ctrl")
  )
glimpse(fdat)
unique(fdat$comparison_id)
fdat

# Create treatmnet effects first
effects <- fdat %>%
  mutate(
    effect = mean_int - mean_ctrl,
    se_effect = sqrt(se_int^2 + se_ctrl^2)
  )

effects |> 
  select(comparison_id,
         arm_name_int,
         arm_name_ctrl,
         author_year_int,
         Outcome,
         post_time_months,
         effect,
         se_effect)
effects

#####################
# Wald ratio estimates
####################
library(dplyr)
library(tidyr)

wald_ratio <- function(dat, outcome, months) {
  
  outcome_col <- paste0("effect_", outcome)
  se_col <- paste0("se_effect_", outcome)
  
  tmp <- dat %>%
    filter(
      Outcome %in% c("BMI", outcome),
      post_time_months == months
    ) %>%
    select(
      comparison_id,
      author_year_int,
      Outcome,
      effect,
      se_effect
    ) %>%
    pivot_wider(
      names_from = Outcome,
      values_from = c(effect, se_effect)
    ) %>%
    filter(
      !is.na(effect_BMI),
      !is.na(.data[[outcome_col]])
    )
  
  beta_x <- tmp$effect_BMI
  se_x <- tmp$se_effect_BMI
  
  beta_y <- tmp[[outcome_col]]
  se_y <- tmp[[se_col]]
  
  tmp %>%
    mutate(
      wald_ratio = beta_y / beta_x,
      se_wald = abs(wald_ratio) *
        sqrt(
          (se_y / beta_y)^2 +
            (se_x / beta_x)^2
        ),
      lci = wald_ratio - 1.96 * se_wald,
      uci = wald_ratio + 1.96 * se_wald
    )
}

#################################################################
# IV estimate BMI and BP (SBP,DBP) at 6 months
################################################################
# BMI and SBP @6 months----
wr_bmi_sbp6 <- wald_ratio(dat = effects, outcome='SBP', months = 6)
wr_bmi_sbp6 <- wr_bmi_sbp6 |>  wald_ratio(
  beta_y = "beta_SBP_6",
  se_y   = "se_SBP_6",
  beta_x = "beta_BMI_6",
  se_x   = "se_BMI_6"
)
wr_bmi_sbp6

# Drop rows wiht NAs
wr_bmi_sbp6_clean <- wr_bmi_sbp6 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  )
wr_bmi_sbp6_clean

# Save rds object and csv----
saveRDS(wr_bmi_sbp6_clean, 'data/glp1ra/processed/wr_bmi_sbp_6m.rds')
write_csv(wr_bmi_sbp6_clean, 'output/glp1ra/tables/wr_bmi_sbp_6m.csv')

# # BMI and DBP @6 months----
wr_bmi_dbp6 <- calc_wr(data = effects,
                          exposure_outcome = 'BMI',
                          exposure_time = 6,
                          outcome_outcome = 'DBP',
                          outcome_time = 6)
wr_bmi_dbp6 <- wr_bmi_dbp6 |>  wald_ratio(
  beta_y = "beta_DBP_6",
  se_y   = "se_DBP_6",
  beta_x = "beta_BMI_6",
  se_x   = "se_BMI_6"
)
wr_bmi_dbp6

# Drop rows wiht NAs
wr_bmi_dbp6_clean <- wr_bmi_dbp6 |> 
  filter(
    !is.na(WR),
    !is.na(WR_SE)
  )
wr_bmi_dbp6_clean

# Save rds object and csv----
saveRDS(wr_bmi_dbp6_clean, 'data/bs/processed/wr_bmi_dbp6m.rds')
write_csv(wr_bmi_dbp6_clean, 'output/bs/tables/wr_bmi_dbp6m.csv')

# BMI and SBP @12, 16, 17 and 24 months----
wr_bmi_sbp12 <- calc_wr(data = effects,
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

