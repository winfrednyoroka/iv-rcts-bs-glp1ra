###############
# Load libraries 
source('R/shared/setup.R')

# Read in the data
bmi_htn <- read_excel('data/bs/raw/BMI_HTN_data.xlsx')
bmi_htn
dat <- bmi_htn
glimpse(dat)
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
    Arm,
    N,
    Hypertension_n,
    BMI_mean
  ) |> 
  pivot_wider(
    names_from = Treatment_type,
    values_from = c(N, Hypertension_n, BMI_mean,Arm)
  )

rd_dat
glimpse(rd_dat)
# Strip off interevention and control from the treatmment arm names
rd_dat$Arm_Intervention <- gsub("_Intervention", "", rd_dat$Arm_Intervention)
rd_dat$Arm_Control <- gsub("_Control", "", rd_dat$Arm_Control)
glimpse(rd_dat)
##################################################################
# Calculate risk difference, se of RD, BMI diff and IV estimate
# Risk difference with continuity correction
##################################################################
rd_dat <- rd_dat |> 
  mutate(
    cc = if_else(
      Hypertension_n_Intervention == 0 |
        Hypertension_n_Intervention == N_Intervention |
        Hypertension_n_Control == 0 |
        Hypertension_n_Control == N_Control,
      0.5, 0
    ),
    
    pI = (Hypertension_n_Intervention + cc) /
      (N_Intervention + 2 * cc),
    
    pC = (Hypertension_n_Control + cc) /
      (N_Control + 2 * cc),
    
    RD = pI - pC,
    
    var_RD =
      pI * (1 - pI) / (N_Intervention + 2 * cc) +
      pC * (1 - pC) / (N_Control + 2 * cc),
    
    SE_RD = sqrt(var_RD),
    
    BMI_diff = BMI_mean_Intervention - BMI_mean_Control,
    
    IV = RD / BMI_diff,
    
    SE_IV = abs(SE_RD / BMI_diff),
    
    VI_IV = SE_IV^2
  )
glimpse(rd_dat)

rd_dat |> 
  select(
    StudyID,
    Time_months,
    BMI_diff,
    RD,
    SE_RD,
    IV,
    SE_IV,
    Arm_Intervention,
    Arm_Control
  )

#############################################################
###### Visualise all estimates without meta-analysing-----
############################################################

rd_dat <- rd_dat |> 
  mutate(
    BMI_diff = BMI_mean_Intervention - BMI_mean_Control,
    
    IV = RD / BMI_diff,
    
    SE_IV = SE_RD / BMI_diff,
    
    lower_IV = IV - 1.96 * SE_IV,
    upper_IV = IV + 1.96 * SE_IV
  )
glimpse(rd_dat)

# Save the plot with no meta-analysis
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


library(metafor)

pdf(
  "output/bs/figures/trialanderror.pdf",
  width = 8,
  height = 6)
forest(
  x = rd_dat$IV,
  ci.lb = rd_dat$lower_IV,
  ci.ub = rd_dat$upper_IV,
  
  slab = paste0(rd_dat$StudyID, " (", rd_dat$Time_months, " mo)"),
  
  ilab = cbind(
    rd_dat$Arm_Intervention,
    rd_dat$Arm_Control
  ),
  
  ilab.xpos = c(-0.4, -0.2),
  
  xlim = c(-1.5, 0.5),
  refline = 0,
  
  xlab = "Increase in hypertension prevalence per 1 kg/m² increase in BMI change"
)

text(-0.4, 7, "Intervention", font = 2)
text(-0.2, 7, "Control", font = 2)
dev.off()
##################################################################################
# Meta-analysis
# Filter out the three studies with hypertension outcome at 12 months
##################################################################################
bmi_htn_12 <- rd_dat |> 
  filter(Time_months == 12)

res_iv <- rma(
  yi = IV,
  vi = VI_IV,
  data = bmi_htn_12,
  method = "FE"
)

summary(res_iv)

pdf(
  "output/bs/figures/bmi_htn_forest.pdf",
  width = 11,
  height = 6)
forest(
  res_iv,
  slab = paste(bmi_htn_12$StudyID,
               bmi_htn_12$Time_months,
               "months"),
  xlab = "Increase in hypertension prevalence per 1 kg/m² increase in BMI"
)
dev.off()

##################################################################
# Save rd_dat to be presented along the forest plot-----
##################################################################
glimpse(rd_dat)
saveRDS(file='data/bs/processed/bmi_htnprocessed.rds',object = rd_dat)
write_csv(x= rd_dat, 'output/bs/tables/bmi_htnRDandIVestimates.csv')
