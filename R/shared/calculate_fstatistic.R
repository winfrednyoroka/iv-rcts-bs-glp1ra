#################################################################################
# Calculate t-statistics and F-statistics comparing intervention and
# control groups for continuous outcomes using summary-level data.
#
# This function expects one row per treatment group within each
# study and follow-up time point. The data are reshaped so that
# intervention and control statistics appear on the same row,
# after which the following quantities are calculated:
#
#   mean_diff = MeanIntervention - MeanControl
#
#   se_diff = Standard error of the difference in means
#
#   t_stat = mean_diff / se_diff
#
#   f_stat = t_stat²
#
# The F-statistic is equivalent to the square of the t-statistic
# for a two-group comparison.
#
# Arguments
# ---------
# df
#   A data frame containing study-level summary statistics.
#
# study_col
#   Column containing the study identifier.
#   Default: "study_id".
#
# time_col
#   Column containing the follow-up time point (months).
#   Default: "post_time_months".
#
# arm_col
#   Column identifying intervention and control groups.
#   Expected values are typically "Intervention" and "Control".
#   Default: "treatment_group".
#
# mean_col
#   Column containing outcome means.
#   Default: "mean".
#
# sd_col
#   Column containing outcome standard deviations.
#   Default: "sd".
#
# n_col
#   Column containing sample sizes.
#   Default: "samplesize".
#
# Returns
# -------
# A tibble containing:
#
#   StudyID
#     Study identifier.
#
#   StudyDuration
#     Follow-up time point.
#
#   mean_diff
#     Difference between intervention and control means.
#
#   se_diff
#     Standard error of the mean difference.
#
#   t_stat
#     Welch-style t-statistic calculated from summary statistics.
#
#   f_stat
#     F-statistic obtained by squaring the t-statistic.
#
# Typical use
# -----------
# Calculate study-level test statistics for:
#   - BMI
#   - SBP
#   - DBP
#
# after combining multi-arm studies where necessary and before
# exploring baseline balance or follow-up differences between
# intervention and control groups.
#################################################################################
calc_fstat <- function(
    df,
    study_col = "study_id",
    time_col = "post_time_months",
    arm_col = "treatment_group",
    mean_col = "mean",
    sd_col = "sd",
    n_col = "samplesize"
) {
  
  df |> 
    select(
      all_of(c(
        study_col,
        time_col,
        arm_col,
        mean_col,
        sd_col,
        n_col
      ))
    ) |> 
    
    pivot_wider(
      id_cols = c(
        all_of(study_col),
        all_of(time_col)
      ),
      names_from = all_of(arm_col),
      values_from = c(
        all_of(mean_col),
        all_of(sd_col),
        all_of(n_col)
      )
    ) |> 
    
    mutate(
      
      mean_diff =
        mean_Intervention -
        mean_Control,
      
      se_diff =
        sqrt(
          (sd_Intervention^2 /
             samplesize_Intervention) +
            (sd_Control^2 /
               samplesize_Control)
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
      mean_diff,
      se_diff,
      t_stat,
      f_stat
    ) |> 
    
    arrange(
      StudyID,
      StudyDuration
    )
  
}