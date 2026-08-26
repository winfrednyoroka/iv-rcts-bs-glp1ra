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

# View select columns
rd_dat |> 
  select(
    StudyID,
    Time_months,
    BMI_diff,
    RD,
    SE_RD,
    IV,
    SE_IV,
    VI_IV,
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
    
    SE_IV = abs(SE_RD / BMI_diff),
    
    lower_IV = IV - 1.96 * SE_IV,
    upper_IV = IV + 1.96 * SE_IV
  )
glimpse(rd_dat)

# Save the plot with no meta-analysis (pdf for presentations and jpeg for manuscript)
pdf(
  "output/bs/figures/bmi_htn_forestnometa-analysis.pdf",
  width = 11,
  height = 5.5
)

par(mar = c(5, 2, 2, 2))

forest(
  x = rd_dat$IV,
  ci.lb = rd_dat$lower_IV,
  ci.ub = rd_dat$upper_IV,
  
  slab = rd_dat$StudyID,
  
  # Text columns
  ilab = cbind(
    rd_dat$Time_months,
    rd_dat$Arm_Intervention,
    rd_dat$Arm_Control
  ),
  
  # These are ALL to the left of the forest plot
  ilab.xpos = c(-.30, -.19, -0.06),
  
  
  xlim = c(-.5, 0.6),
  
  # forest plot
  alim = c(0, 0.4),
  
  at = c(0, 0.1, 0.2, 0.3, 0.4),
  
  refline = 0,
  
  cex = 1.2,
  lwd = 1.2,
  
  xlab = "Increase in hypertension prevalence per 1 kg/m² increase in BMI change"
)

text(-.30, 7, "Time", font = 2)
text(-.19, 7, "Intervention", font = 2)
text(-0.06, 7, "Control", font = 2)

dev.off()

jpeg(
  "output/bs/figures/bmi_htn_forestnometa-analysis.jpeg",
  width = 11,
  height = 5.5,
  units = 'in',
  res = 600
)

par(mar = c(5, 2, 2, 2))

forest(
  x = rd_dat$IV,
  ci.lb = rd_dat$lower_IV,
  ci.ub = rd_dat$upper_IV,
  
  slab = rd_dat$StudyID,
  
  # Text columns
  ilab = cbind(
    rd_dat$Time_months,
    rd_dat$Arm_Intervention,
    rd_dat$Arm_Control
  ),
  
  # These are ALL to the left of the forest plot
  ilab.xpos = c(-.30, -.19, -0.06),
  
  
  xlim = c(-.5, 0.6),
  
  # forest plot
  alim = c(0, 0.4),
  
  at = c(0, 0.1, 0.2, 0.3, 0.4),
  
  refline = 0,
  
  cex = 1.2,
  lwd = 1.2,
  
  xlab = "Increase in hypertension prevalence per 1 kg/m² increase in BMI change"
)

text(-.30, 7, "Time", font = 2)
text(-.19, 7, "Intervention", font = 2)
text(-0.06, 7, "Control", font = 2)

dev.off()


##################################################################################
# Meta-analysis of IV estimates for studies with HTN outcome at 12 months
# Filter out the three studies with hypertension outcome at 12 months
# Save pdf (for ppts) and jpeg (for manuscript) files
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
par(mar = c(5, 2, 2, 2))
forest(
  res_iv,
  slab = bmi_htn_12$StudyID,
  ilab = cbind(
    bmi_htn_12$Time_months,
    bmi_htn_12$Arm_Intervention,
    bmi_htn_12$Arm_Control
  ),
  ilab.xpos = c(-.135, -.08, -0.02),
  refline = 0,
  cex = 1.2,
  lwd = 1.2,
  xlab = "Increase in hypertension prevalence per 1 kg/m² increase in BMI"
)
text(-.135, 5, "Time", font = 2)
text(-.08, 5, "Intervention", font = 2)
text(-0.02, 5, "Control", font = 2)
dev.off()

jpeg(
  "output/bs/figures/bmi_htn_forest.jpeg",
  width = 11,
  height = 6,
  units = 'in',
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(
  res_iv,
  slab = bmi_htn_12$StudyID,
  ilab = cbind(
    bmi_htn_12$Time_months,
    bmi_htn_12$Arm_Intervention,
    bmi_htn_12$Arm_Control
  ),
  ilab.xpos = c(-.135, -.08, -0.02),
  refline = 0,
  cex = 1.2,
  lwd = 1.2,
  xlab = "Increase in hypertension prevalence per 1 kg/m² increase in BMI"
)
text(-.135, 5, "Time", font = 2)
text(-.08, 5, "Intervention", font = 2)
text(-0.02, 5, "Control", font = 2)
dev.off()

##################################################################
# Save rd_dat to be presented along the forest plot-----
##################################################################
glimpse(rd_dat)
saveRDS(file='data/bs/processed/bmi_htnprocessed.rds',object = rd_dat)
write_csv(x= rd_dat, 'output/bs/tables/bmi_htnRDandIVestimates.csv')







