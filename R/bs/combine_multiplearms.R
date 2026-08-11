################################################################################
# Combine multiple study arms into a single group using the
# Cochrane-recommended approach for continuous outcomes.
#
# This function takes summary statistics from two or more arms
# belonging to the same treatment group (e.g., multiple intervention
# arms or multiple control arms) and calculates a combined sample size,
# mean, and standard deviation.
#
# The combined mean is calculated as a sample-size weighted mean:
#
#   mean_comb = Σ(n_i * mean_i) / Σ(n_i)
#
# The combined standard deviation incorporates both:
#   1. Within-arm variability (individual arm SDs), and
#   2. Between-arm variability (differences in arm means).
#
# This follows the method recommended in the Cochrane Handbook for
# combining groups in pairwise meta-analysis.
#
# Arguments:
#   df  A data frame containing at least:
#       - samplesize : sample size for each arm
#       - mean       : mean outcome value for each arm
#       - sd         : standard deviation for each arm
#
# Returns:
#   A tibble with one row containing:
#       - samplesize : combined sample size
#       - mean       : combined mean
#       - sd         : combined standard deviation
#
# Typical use cases:
#   - Combining multiple intervention arms sharing one control group.
#   - Combining multiple control arms sharing one intervention group.
#   - Performing the combination separately by outcome (BMI, SBP, DBP)
#     and follow-up time point.
#
# Assumptions:
#   - Arms being combined belong to the same study.
#   - Measurements are on the same scale.
#   - Means, SDs, and sample sizes are available for all arms.
################################################################################

combine_groups <- function(df) {
  
  N <- sum(df$samplesize)
  
  mean_comb <- weighted.mean(
    df$mean,
    w = df$samplesize
  )
  
  sd_comb <- sqrt(
    (
      sum((df$samplesize - 1) * df$sd^2) +
        sum(df$samplesize * (df$mean - mean_comb)^2)
    ) /
      (N - 1)
  )
  
  tibble(
    samplesize = N,
    mean = mean_comb,
    sd = sd_comb
  )
}