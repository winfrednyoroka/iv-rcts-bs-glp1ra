###################################################################################################
# Script for visualising the outcomes per study duration versus RCT studies ordered by year
# By looking at the dotplot visualisation, I can see BMI, SBP, DBP, HTN separately or combined
# Decision making in IV estimates (BMI measured before BP or at same time point(s))
#
# Initial visualisation of study-level outcome availability and follow-up 
# schedules will be conducted prior to IV analyses to assess evidence coverage,
# identify sparsity across outcomes and time points to inform analysis and reporting strategy.
###################################################################################################

########################################
# Load the libraries and functions----
########################################
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

bs_studyoutcomecoverage_plot = plot_study_outcome_coverage( data =long,
                                              study_order = NULL,
                                              breaks = c(3, 6, 9, 12, 18, 24, 36, 48, 60, 70),
                                              labels = c(
                                                "3", "6", "9", "12",
                                                "18", "24", "36",
                                                "48", "60", "...120"
                                              ),
                                              xlab = "Follow-up time (months)",
                                              ylab = "",
                                              xlab_size = 13,
                                              ylab_size = 13,
                                              axis_text_x_angle = 0,
                                              axis_text_x_size = 12,
                                              axis_text_y_size = 12,
                                              bmi_fill = "grey90",
                                              legend = TRUE)
bs_studyoutcomecoverage_plot

###########################
# Save the plot-------
###########################
ggsave(
  filename = "output/bs/figures/studyoutcome_coverage.jpeg",
  plot = bs_studyoutcomecoverage_plot,
  width = 12,      # inches
  height = 8,      # inches
  units = "in",
  dpi = 600
)


