############################################################################
# fill_female_data
#
# Purpose:
#   Fill missing female participant counts and proportions(*100 pct).
#
# Description:
#   Uses sample size information to complete missing female participant
#   counts or female participant proportions where sufficient data are
#   available.
#
# Rules:
#   1. If female_n is missing and female_prop is available:
#      female_n = female_prop / 100 * sample_size
#
#   2. If female_prop is missing and female_n is available:
#      female_prop = female_n / sample_size * 100
#
# Assumptions:
#   - female_prop is stored as a percentage (e.g., 60, not 0.60).
#   - sample_size is greater than 0 when calculating proportions.
#   - Existing values are not overwritten.
#
# Inputs:
#   data             Data frame containing study-level data.
#   sample_size_col  Name of sample size column.
#   female_n_col     Name of female count column.
#   female_prop_col  Name of female proportion (%) column.
#
# Returns:
#   Data frame with missing female counts and/or proportions filled where
#   possible.
#
# Example:
#   studies <- fill_female_data(
#     studies,
#     sample_size_col = "samplesize",
#     female_n_col = "female_n",
#     female_prop_col = "female_prop"
#   )
############################################################################

## Note for self - round proportion to 2 decimal places

fill_female_data <- function(data,
                             sample_size_col = "samplesize",
                             female_n_col = "female_n",
                             female_prop_col = "female_prop") {
  
  df <- data 
  
  # Fill missing female counts using proportion
  missing_n <- is.na(df[[female_n_col]]) &
    !is.na(df[[female_prop_col]]) &
    !is.na(df[[sample_size_col]])
  
  df[[female_n_col]][missing_n] <- round(
    (df[[female_prop_col]][missing_n] / 100) *
      df[[sample_size_col]][missing_n]
  )
  
  # Fill missing proportions using female count
  missing_prop <- is.na(df[[female_prop_col]]) &
    !is.na(df[[female_n_col]]) &
    !is.na(df[[sample_size_col]]) &
    df[[sample_size_col]] > 0
  
  df[[female_prop_col]][missing_prop] <- round(
    (df[[female_n_col]][missing_prop] /
       df[[sample_size_col]][missing_prop]) * 100,
    2
  )
  
  df
}


