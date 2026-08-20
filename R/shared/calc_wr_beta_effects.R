calc_wr <- function(data,
                    exposure_outcome,
                    exposure_time,
                    outcome_outcome,
                    outcome_time) {
  
  data |> 
    filter(
      (Outcome == exposure_outcome &
         post_time_months == exposure_time) |
        (Outcome == outcome_outcome &
           post_time_months == outcome_time)
    ) |> 
    pivot_wider(
      id_cols = c(study_id,
                  arm_name_Intervention,
                  arm_name_Control,
                  samplesize_Intervention,
                  samplesize_Control),
      names_from = c(Outcome, post_time_months),
      values_from = c(beta, se)
    )
}