################################################################################
# Forest plot for meta-analysis results
#
# Creates a customized forest plot for a random-effects meta-analysis model
# fitted using the metafor package.
#
# The plot displays:
#   - Study-specific effect estimates and 95% confidence intervals
#   - Intervention and control arm labels
#   - A pooled random-effects estimate
#   - Heterogeneity statistics (I², τ², and Cochran's Q)
#
# Arguments
# ---------
# model
#   An rma object returned by metafor::rma().
#
# data
#   A data frame containing:
#     study_id               : Study labels displayed in the forest plot.
#     arm_name_Intervention  : Intervention arm names.
#     arm_name_Control       : Control arm names.
#
#   Rows should correspond to the studies included in the meta-analysis model.
#
# xlab
#   Character string specifying the x-axis label.
#   Default: "Wald Ratio".
#
# Returns
# -------
# No value is returned. The function is called for its side effect of producing
# a forest plot.
#
# Notes
# -----
# The function uses fixed plotting parameters, including:
#   - xlim = c(-22, 10)
#   - ilab.xpos = c(-14, -8)
#   - cex = 0.8
#
# These values may require adjustment for analyses with many studies or long
# study labels.
################################################################################
plot_forest <- function(
    model,
    data,
    xlab = "Wald Ratio"
) {
  
  op <- par(mar = c(4, 4, 2, 2))
  
  forest(
    model,
    
    slab = data$study_id,
    
    ilab = cbind(
      data$arm_name_Intervention,
      data$arm_name_Control
    ),
    
    ilab.xpos = c(-14, -8),
    
    xlim = c(-22, 10),
    
    header = c("Study", "Estimate [95% CI]"),
    
    xlab = xlab,
    
    cex = 1,
    mlab = paste0(
      "Random-effects model (I² = ",
      round(model$I2, 1),
      "%; τ² = ",
      round(model$tau2, 3),
      "; Q = ",
      round(model$QE, 2),
      ")"
    )
  )
  
  text(-14, model$k + 2, "Intervention", font = 2)
  text(-8,  model$k + 2, "Control",      font = 2)
  
  par(op)
}