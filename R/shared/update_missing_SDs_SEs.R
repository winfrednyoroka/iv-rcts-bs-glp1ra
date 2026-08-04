############################################################################
# calc_sd_se_from_ci
#
# Purpose:
#   Calculate standard errors (SE) and standard deviations (SD) from
#   reported confidence intervals and sample sizes for one or more study arms.
#
# Rules:
#   1. Calculate the standard error (SE) from the confidence interval:
#
#        SE = (Upper CI - Lower CI) / (2 * z)
#
#   2. Calculate the standard deviation (SD) from the standard error:
#
#        SD = SE * sqrt(N)
#
#   3. Append the calculated SE and SD values as new columns in the
#      supplied data frame.
#
# Assumptions:
#   - Reported lower and upper bounds correspond to a two-sided confidence
#     interval around the mean.
#   - Confidence intervals are symmetric around the mean.
#   - Sample size is greater than 0.
#   - The default confidence interval is 95% (z = 1.96).
#
# Inputs:
#   data            Data frame containing outcome summary statistics.
#   mean_col        Name of mean column.
#   lower_col       Name of lower confidence interval column.
#   upper_col       Name of upper confidence interval column.
#   n_col           Name of sample size column.
#   ci_level        Confidence level used to derive the z-value.
#                   Default = 0.95.
#
# Returns:
#   Data frame with two additional columns:
#     - se : Estimated standard error.
#     - sd : Estimated standard deviation.
#
# Example:
#   data <- calc_sd_from_ci(
#     data = data,
#     mean_col = "base_meansbp",
#     lower_col = "base_lowersbp",
#     upper_col = "base_uppersbp",
#     n_col = "baseline_N_per_arm",
#     ci_level = 0.95
#   )
#
# Notes:
#   - Useful when studies report means and confidence intervals but do not
#     report standard deviations directly.
############################################################################
calc_sd_se_from_ci <- function(
  data,
  lower_col,
  upper_col,
  n_col,
  se_col,
  sd_col,
  ci_level = 0.95
) {
  z <- qnorm(1 - (1 - ci_level) / 2)

  data |> 
    dplyr::mutate(
      {{ se_col }} := dplyr::if_else(
        is.na({{ se_col }}) &
          !is.na({{ lower_col }}) &
          !is.na({{ upper_col }}),
        ({{ upper_col }} - {{ lower_col }}) /
          (2 * z),
        {{ se_col }}
      ),
      {{ sd_col }} := dplyr::if_else(
        is.na({{ sd_col }}) &
          !is.na({{ se_col }}) &
          !is.na({{ n_col }}),
        {{ se_col }} * sqrt({{ n_col }}),
        {{ sd_col }}
      )
    )
}
