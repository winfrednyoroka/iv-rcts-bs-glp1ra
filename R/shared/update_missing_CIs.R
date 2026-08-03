############################################################################
# update_confidence_bounds
#
# Purpose:
#   Populate missing lower and upper confidence intervals using available
#   mean, SD, SE, and sample size information.
#
# Rules:
#   1. If SE is missing but SD and N are available:
#      SE = SD / sqrt(N)
#
#   2. If SD is missing but SE and N are available:
#      SD = SE * sqrt(N)
#
#   3. If lower_CI or upper_CI are missing:
#      Mean ± z * SE
#
#   
# Assumptions:
#   - 95% confidence interval (z = 1.96).
#   - sample_size is greater than 0 when calculating SEs.
#   - Existing values are not overwritten.
#
# Inputs:
#   data                    Data frame containing baseline data.
#   sample_size_col         Name of sample size column.
#   female_n_col            Name of female count column.
#   female_prop_col         Name of female proportion (%) column.
#   standard_deviation_col  Name of standard deviation column.
#   standard_error_col      Name of standard error column.
#   lower_bound_col         Name of lower confidence bound.
#   upper_bound_col         Name of upper confidence bound.
#   z                       default value set at 1.96.
#   digits                  number of decimal places set at 2.
# Returns:
#   Data frame with missing lower and/or upper bounds filled where
#   possible.
#
# Example:
#   data <- fill_female_data(
#     data,
#     mean_col = "Mean",
#     sample_size_col = 'baseline_N_per_arm'
#     standard_deviation_col = "SD",
#     standard_error_col  = "SE",
#     lower_bound_col = "lower_CI",
#     upper_bound_col = "upper_CI",
#     z = 1.96,
#     digits = 2
#   )
############################################################################

update_confidence_bounds <- function(
    data,
    mean_col = "Mean",
    standard_deviation_col = "SD",
    standard_error_col = "SE",
    sample_size_col = "baseline_N_per_arm",
    lower_bound_col = "lower_CI",
    upper_bound_col = "upper_CI",
    z = 1.96,
    digits = 2
) {
  
  df <- data
  
  # Calculate missing SE
  missing_se <- is.na(df[[standard_error_col]]) &
    !is.na(df[[standard_deviation_col]]) &
    !is.na(df[[sample_size_col]]) &
    df[[sample_size_col]] > 0
  
  df[[standard_error_col]][missing_se] <- round(
    df[[standard_deviation_col]][missing_se] /
    sqrt(df[[sample_size_col]][missing_se]), digits)
  
  # Calculate missing SD
  missing_sd <- is.na(df[[standard_deviation_col]]) &
    !is.na(df[[standard_error_col]]) &
    !is.na(df[[sample_size_col]]) &
    df[[sample_size_col]] > 0
  
  df[[standard_deviation_col]][missing_sd] <- round(
    df[[standard_error_col]][missing_sd] *
    sqrt(df[[sample_size_col]][missing_sd]), digits)
  
  # Lower CI
  missing_lower <- is.na(df[[lower_bound_col]]) &
    !is.na(df[[mean_col]]) &
    !is.na(df[[standard_error_col]])
  
  df[[lower_bound_col]][missing_lower] <- round(
    df[[mean_col]][missing_lower] -
      z * df[[standard_error_col]][missing_lower],
    digits
  )
  
  # Upper CI
  missing_upper <- is.na(df[[upper_bound_col]]) &
    !is.na(df[[mean_col]]) &
    !is.na(df[[standard_error_col]])
  
  df[[upper_bound_col]][missing_upper] <- round(
    df[[mean_col]][missing_upper] +
      z * df[[standard_error_col]][missing_upper],
    digits
  )
  
  df
}


# Test data

test_data <- data.frame(
  Outcome = c("BMI", "BMI", "SBP", "DBP", "BMI"),
  Mean = c(36.4, 35.1, 135.3, 82.1, 37.0),
  SD = c(2.99, NA, 17.35, NA, NA),
  SE = c(NA, 0.40, NA, 1.20, NA),
  baseline_N_per_arm = c(57, 60, 57, 50, NA),
  lower_CI = c(NA, NA, NA, NA, NA),
  upper_CI = c(NA, NA, NA, NA, NA)
)
test_data

update_confidence_bounds(data=test_data,mean_col = 'Mean',standard_deviation_col = 'SD',
                         standard_error_col = 'SE',sample_size_col = 'baseline_N_per_arm',
                         lower_bound_col = 'lower_CI',upper_bound_col = 'upper_CI', z = 1.96,
                         digits = 2)

