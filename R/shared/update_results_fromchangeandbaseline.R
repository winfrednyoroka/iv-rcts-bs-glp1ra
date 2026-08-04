##################################################################################
# Function: create_results_values
#
# Purpose:
# Reconstruct follow-up summary statistics from baseline values and
# change-from-baseline estimates for continuous outcomes.
#
# Description:
# Many studies report baseline means and standard deviations alongside
# mean change-from-baseline and the standard deviation of the change score,
# but do not provide follow-up summary statistics directly.
#
# This function derives follow-up means, standard deviations, standard
# errors, and 95% confidence intervals using the reported baseline and
# change-from-baseline statistics.
#
# The function allows users to specify the relevant column names,
# making it applicable to any continuous outcome (e.g., BMI, SBP, DBP,
# waist circumference, body weight, physical activity measures).
#
# Calculations:
#
# 1. Follow-up Mean
#
#    Follow-up Mean =
#      Baseline Mean + Mean Change from Baseline
#
# 2. Follow-up Standard Deviation
#
#    Follow-up SD =
#
#      sqrt(
#        Baseline SD² +
#        SD of Change² +
#        2 × r × Baseline SD × SD of Change
#      )
#
#    where:
#
#      r = assumed correlation between baseline and follow-up values.
#
# 3. Follow-up Standard Error
#
#    Follow-up SE =
#
#      Follow-up SD / sqrt(n)
#
# 4. Follow-up 95% Confidence Interval
#
#    Lower CI =
#      Follow-up Mean − 1.96 × Follow-up SE
#
#    Upper CI =
#      Follow-up Mean + 1.96 × Follow-up SE
#
# Inputs:
#
#   data
#     A data frame containing baseline and change-from-baseline summary
#     statistics.
#
#   baseline_mean
#     Column containing baseline means.
#
#   baseline_sd
#     Column containing baseline standard deviations.
#
#   mean_change
#     Column containing mean changes from baseline.
#
#   sd_change
#     Column containing standard deviations of change scores.
#
#   sample_size
#     Column containing sample sizes used to calculate follow-up standard
#     errors and confidence intervals.
#
#   r
#     Assumed correlation between baseline and follow-up measurements.
#
#     Must be between -1 and 1.
#
#     Default:
#
#       r = 0.7
#
#   prefix
#     Character string used to name newly created variables.
#
#     Default:
#
#       prefix = "followup"
#
# Generated Variables:
#
#   <prefix>_mean
#     Estimated follow-up mean.
#
#   <prefix>_sd
#     Estimated follow-up standard deviation.
#
#   <prefix>_se
#     Estimated follow-up standard error.
#
#   <prefix>_lower
#     Lower bound of the 95% confidence interval.
#
#   <prefix>_upper
#     Upper bound of the 95% confidence interval.
#
# Returns:
#
#   The original data frame with five additional columns:
#
#     <prefix>_mean
#     <prefix>_sd
#     <prefix>_se
#     <prefix>_lower
#     <prefix>_upper
#
# Assumptions:
#
#   - Change scores are defined as:
#
#       Follow-up Mean − Baseline Mean
#
#   - The correlation between baseline and follow-up measurements is
#     known or can be reasonably approximated.
#
#   - Sample size remains constant between baseline and follow-up.
#
#   - Confidence intervals use the normal approximation:
#
#       Mean ± 1.96 × SE
#
#   - Outcome variables are continuous and measured on the same scale at
#     baseline and follow-up.
#
# Typical Use Cases:
#
#   - Reconstruction of post-intervention means and SDs for
#     meta-analysis.
#
#   - Harmonisation of outcome reporting across studies.
#
#   - Derivation of follow-up values when publications provide only
#     baseline and change-from-baseline statistics.
#
# Example:
#
#   results <- create_results_values(
#     data = dat,
#     baseline_mean = BMI_baseline_mean,
#     baseline_sd   = BMI_baseline_sd,
#     mean_change   = BMI_change_mean,
#     sd_change     = BMI_change_sd,
#     sample_size   = N,
#     r = 0.7,
#     prefix = "BMI_followup"
#   )
#
##################################################################################

create_results_values <- function(
  data,
  baseline_mean,
  baseline_sd,
  mean_change,
  sd_change,
  sample_size,
  r = 0.7,
  prefix = "followup"
) {
  baseline_mean <- enquo(baseline_mean)
  baseline_sd <- enquo(baseline_sd)
  mean_change <- enquo(mean_change)
  sd_change <- enquo(sd_change)
  sample_size <- enquo(sample_size)

  data |> 
    mutate(
      !!paste0(prefix, "_mean") :=
        !!baseline_mean + !!mean_change,
      !!paste0(prefix, "_sd") :=
        sqrt(
          (!!baseline_sd)^2 +
            (!!sd_change)^2 +
            2 * r * !!baseline_sd * !!sd_change
        ),
      !!paste0(prefix, "_se") :=
        .data[[paste0(prefix, "_sd")]] /
          sqrt(!!sample_size),
      !!paste0(prefix, "_lower") :=
        .data[[paste0(prefix, "_mean")]] -
        1.96 * .data[[paste0(prefix, "_se")]],
      !!paste0(prefix, "_upper") :=
        .data[[paste0(prefix, "_mean")]] +
        1.96 * .data[[paste0(prefix, "_se")]]
    )
}
