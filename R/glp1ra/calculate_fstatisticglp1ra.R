################################################################################
# Calculate arm-specific t- and F-statistics for multi-arm GLP1RA trials.
#
# Assumes:
#   - One control arm per study/timepoint.
#   - One or more intervention arms per study/timepoint.
#   - Control groups may be reused across multiple intervention comparisons.
#
# Returns one row per intervention arm.
################################################################################

calc_fstat_multiarm <- function(
    df,
    study_col = "study_id",
    time_col = "post_time_months",
    arm_col = "treatment_group",
    arm_name_col = "arm_name",
    mean_col = "mean",
    sd_col = "sd",
    n_col = "samplesize"
) {
  
  dat <- df |>
    select(
      all_of(c(
        study_col,
        time_col,
        arm_col,
        arm_name_col,
        mean_col,
        sd_col,
        n_col
      ))
    )
  
  controls <- dat |>
    filter(.data[[arm_col]] == "Control") |>
    rename(
      ControlArm = all_of(arm_name_col),
      mean_ctrl = all_of(mean_col),
      sd_ctrl = all_of(sd_col),
      n_ctrl = all_of(n_col)
    )
  
  interventions <- dat |>
    filter(.data[[arm_col]] == "Intervention") |>
    rename(
      InterventionArm = all_of(arm_name_col),
      mean_int = all_of(mean_col),
      sd_int = all_of(sd_col),
      n_int = all_of(n_col)
    )
  
  interventions |>
    left_join(
      controls,
      by = c(study_col, time_col)
    ) |>
    
    mutate(
      
      mean_diff =
        mean_int - mean_ctrl,
      
      se_diff =
        sqrt(
          (sd_int^2 / n_int) +
            (sd_ctrl^2 / n_ctrl)
        ),
      
      t_stat =
        mean_diff / se_diff,
      
      f_stat =
        t_stat^2
      
    ) |>
    
    rename(
      StudyID = all_of(study_col),
      StudyDuration = all_of(time_col)
    ) |>
    
    select(
      StudyID,
      StudyDuration,
      InterventionArm,
      ControlArm,
      mean_diff,
      se_diff,
      t_stat,
      f_stat
    ) |>
    
    arrange(
      StudyID,
      StudyDuration,
      InterventionArm
    )
  
}