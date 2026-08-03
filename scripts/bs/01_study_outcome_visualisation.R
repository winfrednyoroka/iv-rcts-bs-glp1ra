###################################################################################################
# Script for visualising the outcomes per study duration versus RCT studies ordered by year
# By looking at the dotplot visualisation, I can see BMI, SBP, DBP, HTN separately or combined
# Decision making in IV estimates (BMI measured before BP or at same time point(s))
#
# Initial visualisation of study-level outcome availability and follow-up 
# schedules will be conducted prior to IV analyses to assess evidence coverage,
# identify sparsity across outcomes and time points to inform analysis and reporting strategy.
###################################################################################################
# Load the function
source('R/shared/setup.R')
source('R/shared/followup_evidence_mapping.R')

##############################
# Read in the data----
#############################

study_outcomes <- read_excel('./data/bs/processed/ExploratoryDataAnalysis.xlsx', sheet = 'StudyID_Followupmatrix')
glimpse(study_outcomes)

# Mutate the Study column 
study_outcomes <- study_outcomes |> 
  mutate(Study = str_replace(Study, "_(\\d{4})$", " (\\1)"))

study_outcomes
# Convert to long format----
long <- study_outcomes |> 
  pivot_longer(
    cols = -c(Study,Year),
    names_to = "Followup",
    values_to = "Outcome"
  ) |> 
  filter(!is.na(Outcome))
long
########################################################
# Convert the months from strings to  actual months----
########################################################
long <- long |> 
  mutate(
    Month = readr::parse_number(Followup) # Parses the first number it finds and drops any non-numeric characters
  )
long
########################################################
# Create indicator variables/create flags----
########################################################
long <- long |> 
  mutate(
    BMI = str_detect(Outcome, "BMI"),
    SBP = str_detect(Outcome, "SBP"),
    DBP = str_detect(Outcome, "DBP"),
    HTN = str_detect(Outcome, "HTN")
  )
long
########################################################
# Order studies by publication year----
########################################################
study_order <- long |> 
  distinct(Study, Year) |> 
  arrange(Year, Study)
study_order

long <- long |> 
  mutate(
    Study = factor(
      Study,
      levels = rev(study_order$Study)
    )
  )
long
########################################################
# Check that the order is as expected----
########################################################
long |> 
  select(Study, Year, Month, Outcome) |> 
  arrange(Year, Study, Month)


################################################
# Create a placeholder for 120m at 70m----
###############################################
long

long <- long |> 
  mutate(
    Month_plot = case_when(
      Month == 120 ~ 70,
      TRUE ~ Month
    )
  )
glimpse(long)

#####################################################
# Visualise outcome-time points map----
####################################################
