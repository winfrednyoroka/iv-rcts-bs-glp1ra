################################################################################
# Script for combining multiarms----
# Handle, SBP, DBP and BMI outcomes
################################################################################

# Load the libraries and functions
source('R/shared/setup.R')
source('R/bs/combine_multiplearms.R')

# Read in the data
baseline_post <- readRDS('data/bs/processed/baseline_post_relabelmultiarms.rds')
glimpse(baseline_post)

################################################################################
combined_data <- baseline_post |> 
  mutate(
    study_id = str_extract(
      comparison_id,
      "^.*?\\([0-9]{4}\\)"
    )
  ) |> 
  group_by(
    study_id,
    Outcome,
    post_time_months
  ) |> 
  group_modify(~{
    
    n_int <- n_distinct(
      .x$arm_name[
        .x$treatment_group == "Intervention"
      ]
    )
    
    n_con <- n_distinct(
      .x$arm_name[
        .x$treatment_group == "Control"
      ]
    )
    
    if (n_int > 1 & n_con == 1) {
      
      bind_rows(
        
        combine_groups(
          filter(
            .x,
            treatment_group == "Intervention"
          )
        ) |> 
          mutate(
            treatment_group = "Intervention",
            arm_name = paste(
              unique(
                .x$arm_name[
                  .x$treatment_group == "Intervention"
                ]
              ),
              collapse = " + "
            )
          ),
        
        filter(
          .x,
          treatment_group == "Control"
        ) |> 
          distinct(
            treatment_group,
            arm_name,
            samplesize,
            mean,
            sd
          )
      )
      
    } else if (n_con > 1 & n_int == 1) {
      
      bind_rows(
        
        filter(
          .x,
          treatment_group == "Intervention"
        ) |> 
          distinct(
            treatment_group,
            arm_name,
            samplesize,
            mean,
            sd
          ),
        
        combine_groups(
          filter(
            .x,
            treatment_group == "Control"
          )
        ) |> 
          mutate(
            treatment_group = "Control",
            arm_name = paste(
              unique(
                .x$arm_name[
                  .x$treatment_group == "Control"
                ]
              ),
              collapse = " + "
            )
          )
      )
      
    } else {
      
      .x |> 
        distinct(
          treatment_group,
          arm_name,
          samplesize,
          mean,
          sd
        )
    }
    
  }) |> 
  ungroup()

glimpse(combined_data)

################################################################
# Save the data as RDS object to be used by other scripts-----
# Instrument strength assessment, IV analysis
################################################################
saveRDS(combined_data, 'data/bs/processed/baseline_post_combinedmultiarms.rds')
