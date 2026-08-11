##########################################################################################
# Script for identifying studies with multiple arms
# Detects these multiple arms and creates new labels
# Multi-intervention or control, add labels to the tune of A, B, C, D, etc 
# depending on number of arms
# Save as rds object to be used for data driven visualisation and IV estimation
#########################################################################################
#################################################
# Load libraries and custom functions----
#################################################
source("R/shared/setup.R")

####################################################
# Read in the data ----
###################################################
baseline_post <- readRDS('data/bs/processed/post_results_updated.rds')
glimpse(baseline_post)

# Create a author_year column
baseline_post$author_year <- paste0(baseline_post$last_name," (", baseline_post$publication_year, ")")
glimpse(baseline_post)
unique(baseline_post$Outcome)

# Make short readable treatment arm names
baseline_post <- baseline_post |> 
  mutate(
    arm_name  = ARMID |> 
      str_remove("^\\d+_") |> 
      str_remove("_(Intervention|Control)$")
  )
glimpse(baseline_post)


##########################################################
# Create labels (A/B) for multi-arm studies ----
##########################################################
##########################################################
# Create comparison lookup table ----
##########################################################

arm_lookup <- baseline_post |> 
  distinct(
    ID,
    author_year,
    arm_name,
    treatment_group
  )
glimpse(arm_lookup)

comparison_lookup <-
  
  arm_lookup |> 
  
  group_by(ID, author_year) |> 
  
  group_split() |> 
  
  purrr::map_dfr(function(x) {
    
    interventions <- x |> 
      filter(treatment_group == "Intervention") |> 
      arrange(arm_name)
    
    controls <- x |> 
      filter(treatment_group == "Control") |> 
      arrange(arm_name)
    
    # Skip studies that do not have both groups
    if (nrow(interventions) == 0 | nrow(controls) == 0) {
      return(tibble())
    }
    
    comparisons <-
      
      tidyr::expand_grid(
        intervention_arm = interventions$arm_name,
        control_arm = controls$arm_name
      )
    
    n_comparisons <- nrow(comparisons)
    
    comparisons |> 
      
      mutate(
        
        ID = first(x$ID),
        
        author_year = first(x$author_year),
        
        comparison_letter = if (
          n_comparisons > 1
        ) {
          LETTERS[seq_len(n_comparisons)]
        } else {
          ""
        },
        
        comparison_id = paste0(
          author_year,
          comparison_letter,
          " ",
          intervention_arm,
          " vs ",
          control_arm
        )
      ) |> 
      
      select(
        ID,
        author_year,
        intervention_arm,
        control_arm,
        comparison_id
      )
    
  })

comparison_lookup
comparison_lookup |> 
  select(comparison_id) |> 
  distinct()

########################
## Expand intervention arms
intervention_rows <-
  
  baseline_post |> 
  
  inner_join(
    comparison_lookup |> 
      select(
        ID,
        comparison_id,
        intervention_arm
      ),
    by = c(
      "ID",
      "arm_name" = "intervention_arm"
    )
  )
intervention_rows

control_rows <-
  
  baseline_post |> 
  
  inner_join(
    comparison_lookup |> 
      select(
        ID,
        comparison_id,
        control_arm
      ),
    by = c(
      "ID",
      "arm_name" = "control_arm"
    )
  )
control_rows

expanded_data <-
  
  bind_rows(
    intervention_rows,
    control_rows
  ) |> 
  
  distinct()
expanded_data
# check the number of comparisons
expanded_data |> 
  count(comparison_id)
glimpse(expanded_data)

##########################################################
# Create baseline values ----
##########################################################
baseline_rows <-
  
  expanded_data |> 
  
  distinct(
    comparison_id,
    arm_name,
    Outcome,
    .keep_all = TRUE
  ) |> 
  
  transmute(
    
    comparison_id,
    author_year,
    arm_name,
    treatment_group,
    Outcome,
    baseline_N_per_arm,
    post_time_months = 0,
    
    mean = baseline_mean,
    lowerbound = baseline_lowerbound,
    upperbound = baseline_upperbound,
    sd = baseline_sd,
    se = baseline_se
  )
baseline_rows <- baseline_rows |> 
  rename('samplesize' = 'baseline_N_per_arm')
baseline_rows
##########################################################
# Create follow-up values ----
##########################################################
followup_rows <-
  
  expanded_data |> 
  
  transmute(
    
    comparison_id,
    author_year,
    arm_name,
    treatment_group,
    Outcome,
    post_samplesize,
    post_time_months,
    
    mean = post_mean,
    lowerbound = post_lowerbound,
    upperbound = post_upperbound,
    sd = post_sd,
    se = post_se
  )
followup_rows
followup_rows <- followup_rows |> 
  rename('samplesize' = 'post_samplesize')
followup_rows

##########################################################
# Combine Baseline and Follow-up values ----
##########################################################
plot_data <-
  bind_rows(
    baseline_rows,
    followup_rows
  ) |> 
  
  arrange(
    comparison_id,
    Outcome,
    arm_name,
    post_time_months
  )
glimpse(plot_data)

#####################################
# Save the data to be used in data driven visualisation ----
# Baseline vs post-intervention
# Euclidean distance
# T-test 4 hypothesis testing
############################################################
saveRDS(plot_data, file = 'data/bs/processed/baseline_post_relabelmultiarms.rds')
