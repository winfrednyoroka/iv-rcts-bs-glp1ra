# Results level risk of bias assessments for bariatric surgery
# Use robVis to visualise the bias assessments

source('R/shared/setup.R')

# Load the data
rob_bs <- read_excel('data/glp1ra/raw/Robassessments_GLP1RAs.xlsm', sheet = 'Summary', skip = 3 )
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
sbp_rob <- rob_bs |> 
  filter(Outcome == "SBP")
dbp_rob <- rob_bs |> 
  filter(Outcome == "DBP")
glimpse(sbp_rob)

# Drop the last two columns
sbp_rob <- sbp_rob[, 1:(ncol(sbp_rob)-2)]
dbp_rob <- dbp_rob[, 1:(ncol(dbp_rob)-2)]
sbp_rob


# Hypertension
# Summary
summary_rob <- rob_summary(data = sbp_rob, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = sbp_rob,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/glp1ra/figures/robvisualisation/sbp_rob_fig.jpeg")

# SBP
# Summary
summary_rob <- rob_summary(data = dbp_rob, tool = "Generic")
summary_rob
# Traffic lights
trafficlight_rob <- rob_traffic_light(data = dbp_rob,
                                      tool = "Generic",
                                      psize = 10)

rob_save(trafficlight_rob, "output/glp1ra/figures/robvisualisation/dbp_rob_fig.jpeg")