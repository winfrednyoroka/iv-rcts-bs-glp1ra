####################################################################################
# Function: impute_change_stats
#
# Purpose:
# Impute missing change-from-baseline summary statistics using available
# combinations of mean, confidence interval (CI), standard deviation (SD),
# standard error (SE), and sample size (n).
#
# Description:
# The function processes each row independently and applies a series of
# deterministic rules to derive missing statistics where sufficient
# information is available. Existing non-missing values are preserved.
#
# Imputation Rules:
#
# 1. Mean + CI + n -> SE -> SD
#    SE = (Upper CI - Lower CI) / (2 × z)
#    SD = SE × sqrt(n)
#
# 2. Mean + SD + n -> SE -> CI
#    SE = SD / sqrt(n)
#    Lower CI = Mean - z × SE
#    Upper CI = Mean + z × SE
#
# 3. Mean + SE + n -> SD -> CI
#    SD = SE × sqrt(n)
#    Lower CI = Mean - z × SE
#    Upper CI = Mean + z × SE
#
# 4. Rows with insufficient information are left unchanged.
#
# Confidence Intervals:
# The confidence level is taken from `ci_level_col` when available; otherwise
# `default_ci` is used. Corresponding z-values are calculated from the
# standard normal distribution.
#
# Inputs:
#   data          Data frame containing change-from-baseline statistics.
#   mean_col      Mean change column.
#   lower_col     Lower confidence interval column.
#   upper_col     Upper confidence interval column.
#   sd_col        Standard deviation column.
#   se_col        Standard error column.
#   n_col         Sample size column.
#   ci_level_col  Confidence level column (%).
#   default_ci    Default CI level if unavailable (default = 95).
#
# Returns:
# A data frame with imputed SD, SE, and/or confidence interval values where
# derivation is possible from the available data.
#
# Assumptions:
# - Confidence intervals are symmetric around the mean.
# - CI calculations use the standard normal distribution (z-values).
# - Sample size is required for SD-SE conversions.
# - Existing values are not overwritten.
#####################################################################################


# Impute change statistics
impute_change_stats <- function(data,
                                mean_col = "meanChangefrombaseline",
                                lower_col = "lowerCIChangefrombaseline",
                                upper_col = "upperCIChangefrombaseline",
                                sd_col = "SDchangefrombaseline",
                                se_col = "SEofChangefrombaseline",
                                n_col = "SampleSize",
                                ci_level_col = "CI_level (%)",
                                default_ci = 95) {
  
  z_from_ci <- function(ci_level) {
    stats::qnorm(1 - (1 - ci_level / 100) / 2)
  }
  
  df <- data
  
  for (i in seq_len(nrow(df))) {
    
    mean_val  <- df[[mean_col]][i]
    lower_val <- df[[lower_col]][i]
    upper_val <- df[[upper_col]][i]
    sd_val    <- df[[sd_col]][i]
    se_val    <- df[[se_col]][i]
    n_val     <- df[[n_col]][i]
    
    ci_level <- if (!is.null(ci_level_col) &&
                    ci_level_col %in% names(df) &&
                    !is.na(df[[ci_level_col]][i])) {
      df[[ci_level_col]][i]
    } else {
      default_ci
    }
    
    z <- z_from_ci(ci_level)
    
    ####################################################################
    # Rule 1:
    # Mean + CI + n -> SE -> SD
    ####################################################################
    if (!is.na(mean_val) &&
        !is.na(lower_val) &&
        !is.na(upper_val) &&
        !is.na(n_val)) {
      
      if (is.na(se_val)) {
        se_val <- (upper_val - lower_val) / (2 * z)
        df[[se_col]][i] <- se_val
      }
      
      if (is.na(sd_val)) {
        sd_val <- se_val * sqrt(n_val)
        df[[sd_col]][i] <- sd_val
      }
    }
    
    ####################################################################
    # Rule 2:
    # Mean + SD + n -> SE -> CI
    ####################################################################
    if (!is.na(mean_val) &&
        !is.na(sd_val) &&
        !is.na(n_val)) {
      
      if (is.na(se_val)) {
        se_val <- sd_val / sqrt(n_val)
        df[[se_col]][i] <- se_val
      }
      
      if (is.na(lower_val)) {
        df[[lower_col]][i] <- mean_val - z * se_val
      }
      
      if (is.na(upper_val)) {
        df[[upper_col]][i] <- mean_val + z * se_val
      }
    }
    
    ####################################################################
    # Rule 3:
    # Mean + SE + n -> SD -> CI
    ####################################################################
    if (!is.na(mean_val) &&
        !is.na(se_val) &&
        !is.na(n_val)) {
      
      if (is.na(sd_val)) {
        df[[sd_col]][i] <- se_val * sqrt(n_val)
      }
      
      if (is.na(lower_val)) {
        df[[lower_col]][i] <- mean_val - z * se_val
      }
      
      if (is.na(upper_val)) {
        df[[upper_col]][i] <- mean_val + z * se_val
      }
    }
    ####################################################################
    # Rule 4 & 5 automatically leave values missing
    ####################################################################
  }
  
  df
}
