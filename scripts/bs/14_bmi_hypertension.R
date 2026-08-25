###############
# Load libraries 
source('R/shared/setup.R')

# Read in the data
bmi_htn <- read_excel('data/bs/raw/BMI_HTN_data.xlsx')
bmi_htn
# Continuity correction----
dat <- bmi_htn |>
  dplyr::mutate(
    Hypertension_n = dplyr::if_else(
      StudyID == "Azevedo (2018)" & Time_months == 12,
      Hypertension_n + 0.5,
      Hypertension_n
    )
  )
dat
# Schauer intervention arms
schauer_int <- dat |> 
  filter(
    StudyID == "Schauer (2012)",
    Treatment_type == "Intervention"
  )
schauer_int
# Combined mean
combined_mean <- weighted.mean(
  schauer_int$BMI_mean,
  schauer_int$N
)

# Combined SD
combined_sd <- sqrt(
  (
    sum((schauer_int$N - 1) * schauer_int$BMI_SD^2) +
      sum(schauer_int$N * (schauer_int$BMI_mean - combined_mean)^2)
  ) /
    (sum(schauer_int$N) - 1)
)

combined_row <- data.frame(
  StudyID = "Schauer (2012)",
  Time_months = 12,
  Arm = "SG_MT_RYGB_MT",
  Treatment_type = "Intervention",
  N = sum(schauer_int$N),
  BMI_mean = combined_mean,
  BMI_SD = combined_sd,
  Hypertension_n = sum(schauer_int$Hypertension_n)
)

combined_row

dat_new <- dat |> 
  filter(!(StudyID == "Schauer (2012)" &
             Treatment_type == "Intervention")) |> 
  bind_rows(combined_row) |> 
  arrange(StudyID, Time_months, Treatment_type)
dat_new

# Study_time dataset

rd_dat <- dat_new |> 
  select(
    StudyID,
    Time_months,
    Treatment_type,
    N,
    Hypertension_n,
    BMI_mean
  ) |> 
  pivot_wider(
    names_from = Treatment_type,
    values_from = c(N, Hypertension_n, BMI_mean)
  )

rd_dat
glimpse(rd_dat)

# Calculate risk difference, se of RD, BMI diff and IV estimate
rd_dat <- rd_dat |> 
  mutate(
    
    # Hypertension proportions
    pI = Hypertension_n_Intervention / N_Intervention,
    pC = Hypertension_n_Control / N_Control,
    
    # Risk difference
    RD = pI - pC,
    
    # Variance and SE of RD
    var_RD =
      pI*(1-pI)/N_Intervention +
      pC*(1-pC)/N_Control,
    
    SE_RD = sqrt(var_RD),
    
    # BMI contrast
    BMI_diff = BMI_mean_Intervention - BMI_mean_Control,
    
    # IV estimate (Wald ratio)
    IV = RD / BMI_diff,
    
    # Approximate SE of IV estimate
    SE_IV = SE_RD / BMI_diff,
    
    VI_IV = SE_IV^2
  )

rd_dat |> 
  select(
    StudyID,
    Time_months,
    BMI_diff,
    RD,
    SE_RD,
    IV,
    SE_IV
  )
# Meta-analysis
library(metafor)

res_iv <- rma(
  yi = IV,
  vi = VI_IV,
  data = rd_dat,
  method = "REML"
)

summary(res_iv)

pdf(
  "output/bs/figures/bmi_htn_forest.pdf",
  width = 11,
  height = 6)
forest(
  res_iv,
  slab = paste(rd_dat$StudyID,
               rd_dat$Time_months,
               "months"),
  xlab = "Reduction in hypertension risk per 1 kg/m² Reduction in BMI"
)
dev.off()
res <- rma(
  yi = RD,
  vi = vi,
  slab = rd_dat$StudyID,
  data = rd_dat,
  method = "REML"
)

summary(res)
forest(res)


###### Visualise estimates without meta-analysing-----

rd_dat <- rd_dat |> 
  mutate(
    BMI_diff = BMI_mean_Intervention - BMI_mean_Control,
    
    IV = RD / BMI_diff,
    
    SE_IV = SE_RD / BMI_diff,
    
    lower_IV = IV - 1.96 * SE_IV,
    upper_IV = IV + 1.96 * SE_IV
  )
glimpse(rd_dat)

# Save the plot
pdf(
  "output/bs/figures/bmi_htn_forestnometa-analysis.pdf",
  width = 8,
  height = 6)
forest(
  x = rd_dat$IV,
  ci.lb = rd_dat$lower_IV,
  ci.ub = rd_dat$upper_IV,
  
  slab = paste0(
    rd_dat$StudyID,
    " (",
    rd_dat$Time_months,
    " months)"
  ),
  
  xlab = "Increase in hypertension prevalence per 1 kg/m² increase in BMI change",
  
  refline = 0
)
dev.off()
