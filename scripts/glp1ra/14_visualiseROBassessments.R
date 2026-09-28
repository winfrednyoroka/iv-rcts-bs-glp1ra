# Results level risk of bias assessments for bariatric surgery
# Use robVis to visualise the bias assessments

source('R/shared/setup.R')

# Load the data
rob_bs <- read_excel('data/glp1ra/raw/Robassessments_GLP1RAs.xlsm', sheet = 'Summary', skip = 3 )
glimpse(rob_bs)

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
# SBP
sbp_rob_6m <- rob_bs |> 
  filter(Outcome == "SBP" & Time == "6m")
sbp_rob_12_24m <- rob_bs |> 
  filter(Outcome == "SBP" & Time == "12-24m")

# DBP
dbp_rob_6m <- rob_bs |> 
  filter(Outcome == "DBP" & Time == "6m")
dbp_rob_12_24m <- rob_bs |> 
  filter(Outcome == "DBP" & Time == "12-24m")

glimpse(sbp_rob_6m)

# Drop the last two columns
sbp_rob_6m <- sbp_rob_6m[, 1:(ncol(sbp_rob_6m)-3)]
sbp_rob_12_24m <- sbp_rob_12_24m[, 1:(ncol(sbp_rob_12_24m)-3)]

dbp_rob_6m <- dbp_rob_6m[, 1:(ncol(dbp_rob_6m)-3)]
dbp_rob_12_24m <- dbp_rob_12_24m[, 1:(ncol(dbp_rob_12_24m)-3)]

glimpse(sbp_rob_6m)


#SBP----
# BMI and SBP at 6 months only
# Summary
summary_rob <- rob_summary(data = sbp_rob_6m, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = sbp_rob_6m,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/glp1ra/figures/robvisualisation/sbp_6m_rob_fig.jpeg") 

# BMI and SBP from 12 through 24 months
# Summary
summary_rob <- rob_summary(data = sbp_rob_12_24m, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = sbp_rob_12_24m,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/glp1ra/figures/robvisualisation/sbp_12_24m_rob_fig.jpeg")

# DBP----
# BMI and DBP at 6 months only
# Summary
summary_rob <- rob_summary(data = dbp_rob_6m, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = dbp_rob_6m,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/glp1ra/figures/robvisualisation/dbp_6m_rob_fig.jpeg") 

# BMI and DBP at 12 through 24 months---
# Summary
summary_rob <- rob_summary(data = dbp_rob_12_24m, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = dbp_rob_12_24m,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/glp1ra/figures/robvisualisation/dbp_12_24m_rob_fig.jpeg")
