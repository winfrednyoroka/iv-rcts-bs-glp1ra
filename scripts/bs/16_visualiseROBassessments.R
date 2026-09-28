# Results level risk of bias assessments for bariatric surgery
# Use robVis to visualise the bias assessments

source('R/shared/setup.R')

# Load the data
rob_bs <- read_excel('data/bs/raw/Robassessments_BS.xlsm', sheet = 'Summary', skip = 3 )
rob_bs

# Rename the first column from ARMID to Study
rob_bs <- rob_bs |> 
  rename("Study" = ARMID, "Overall" = `Overall risk of bias`,
         "Bias due to weak instrument" = DOMAIN1,
         "Bias due to confounding" = DOMAIN2,
         "Bias due to alternative mediating pathways" = DOMAIN3,
         "Bias due to violation of assumptions required  for estimation of target parameter" = DOMAIN4,
         "Bias due to selection of participants into the study" = DOMAIN5,
         "Bias in classification of the instrument" = DOMAIN6,
         "Bias in measurement of exposure" = DOMAIN7,
         "Bias due to post-exposure interventions" = DOMAIN8,
         "Bias due to missing data" = DOMAIN9,
         "Bias due to measurement of outcome" = DOMAIN10,
         "Bias due to selection of the reported result" = DOMAIN11)
# Filter the findings based on outcome column
# Hypertension----
htn_rob <- rob_bs |> 
  filter(Outcome == "Hypertension")
htn_rob_12m <- rob_bs |> 
  filter(Outcome == "Hypertension" & Time == "12m" )
# SBP----
sbp_rob_12m <- rob_bs |> 
  filter(Outcome == "SBP" & Time == "12-12m")
sbp_rob_12_24m <- rob_bs |> 
  filter(Outcome == "SBP" & Time == "12-24m")
sbp_rob_24_24m <- rob_bs |> 
  filter(Outcome == "SBP" & Time == "24-24m")

# DBP----
dbp_rob_12m <- rob_bs |> 
  filter(Outcome == "DBP" & Time == "12-12m")
dbp_rob_12_24m <- rob_bs |> 
  filter(Outcome == "DBP" & Time == "12-24m")
dbp_rob_24_24m <- rob_bs |> 
  filter(Outcome == "DBP" & Time == "24-24m")
glimpse(htn_rob)

# Drop the last two columns
htn_rob <- htn_rob[, 1:(ncol(htn_rob)-3)]
htn_rob_12m <- htn_rob_12m[, 1:(ncol(htn_rob_12m)-3)]
# SBP
sbp_rob_12m <- sbp_rob_12m[, 1:(ncol(sbp_rob_12m)-3)]
sbp_rob_12_24m <- sbp_rob_12_24m[, 1:(ncol(sbp_rob_12_24m)-3)]
sbp_rob_24_24m <- sbp_rob_24_24m[, 1:(ncol(sbp_rob_24_24m)-3)]
# DBP
dbp_rob_12m <- dbp_rob_12m[, 1:(ncol(dbp_rob_12m)-3)]
dbp_rob_12_24m <- dbp_rob_12_24m[, 1:(ncol(dbp_rob_12_24m)-3)]
dbp_rob_24_24m <- dbp_rob_24_24m[, 1:(ncol(dbp_rob_24_24m)-3)]

glimpse(htn_rob)
htn_rob


# Hypertension
# All time periods - 3months to 24 months
# Summary
summary_rob <- rob_summary(data = htn_rob, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = htn_rob,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/bs/figures/robvisualisation/htn_rob_fig.jpeg")
# 12 months toime point only
# Summary
summary_rob <- rob_summary(data = htn_rob_12m, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = htn_rob_12m,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/bs/figures/robvisualisation/htn_12m_rob_fig.jpeg")

# SBP----
# BMI and SBP at 12 months
# Summary
summary_rob <- rob_summary(data = sbp_rob_12m, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = sbp_rob_12m,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/bs/figures/robvisualisation/sbp_12m_rob_fig.jpeg")

# BMI at 12 months and SBP at 24 months
# Summary
summary_rob <- rob_summary(data = sbp_rob_12_24m, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = sbp_rob_12_24m,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/bs/figures/robvisualisation/sbp_12_24m_rob_fig.jpeg")

# BMI at 24 months and SBP at 24 months
# Summary
summary_rob <- rob_summary(data = sbp_rob_24_24m, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = sbp_rob_24_24m,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/bs/figures/robvisualisation/sbp_24_24m_rob_fig.jpeg")

# Diastolic blood pressure (DBP) ----
# SBP----
# BMI and SBP at 12 months
# Summary
summary_rob <- rob_summary(data = dbp_rob_12m, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = dbp_rob_12m,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/bs/figures/robvisualisation/dbp_12m_rob_fig.jpeg")

# BMI at 12 months and SBP at 24 months
# Summary
summary_rob <- rob_summary(data = dbp_rob_12_24m, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = dbp_rob_12_24m,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/bs/figures/robvisualisation/dbp_12_24m_rob_fig.jpeg")

# BMI at 24 months and SBP at 24 months
# Summary
summary_rob <- rob_summary(data = dbp_rob_24_24m, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = dbp_rob_24_24m,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/bs/figures/robvisualisation/dbp_24_24m_rob_fig.jpeg")

