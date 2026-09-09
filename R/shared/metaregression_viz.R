###############################################################################
# Function: plot_meta_regression()
#
# Purpose:
# Creates a scatter plot of study-level effect estimates and overlays the
# fitted regression line from a previously fitted metafor meta-regression model.
#
# The function is designed for visualising associations between treatment
# effects (e.g., BMI differences and blood pressure differences) across studies.
#
# Arguments:
#   data       : Data frame containing the plotting variables.
#   xvar       : Character string giving the name of the moderator variable
#                to plot on the x-axis (e.g., "beta_BMI_12").
#   yvar       : Character string giving the name of the outcome effect
#                estimate to plot on the y-axis (e.g., "beta_SBP_12").
#   linear_model      : Previously fitted metafor::rma() meta-regression model.
#   quad_model      : Previously fitted metafor::rma() meta-regression model.
#   study_var  : Optional character string containing study labels to be
#                displayed beside each data point.
#   xlab       : Optional x-axis label.
#   ylab       : Optional y-axis label.
#   line_col   : Colour of the fitted regression line.
#   point_col  : Colour of plotted study points.
#
# Details:
#   - A scatter plot of study-specific effect estimates is produced.
#   - If study labels are supplied, labels are displayed beside points.
#   - The label for the rightmost point is positioned to the left to reduce
#     text clipping at the plot boundary.
#   - The fitted meta-regression line is obtained using predict() on the
#     supplied model and overlaid on the plot.
#   - Confidence intervals around the regression line are not displayed.
#
# Example:
#
# bmi_sbp_12_res <- rma(
#   yi = beta_SBP_12,
#   vi = vi,
#   mods = ~ beta_BMI_12 + beta_BMI_12^2,
#   data = bmi12_sbp12,
#   method = "FE"
# )
#
# plot_meta_regression(
#   data = bmi12_sbp12,
#   xvar = "beta_BMI_12",
#   yvar = "beta_SBP_12",
#   linear_model = bmi_sbp_12_res,
#   quad_model = bmi_sbp_12_res_quad,
#   study_var = "study",
#   xlab = "BMI difference at 12 months (kg/m²)",
#   ylab = "SBP difference at 12 months (mmHg)"
# )
#
###############################################################################
plot_meta_regression <- function(data,
                                 xvar,
                                 yvar,
                                 linear_model,
                                 quadratic_model = NULL,
                                 study_var = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 linear_col = "green",
                                 quadratic_col = "red",
                                 point_col = "black") {
  
  x <- data[[xvar]]
  y <- data[[yvar]]
  
  plot(
    x, y,
    pch = 19,
    col = point_col,
    xlab = ifelse(is.null(xlab), xvar, xlab),
    ylab = ifelse(is.null(ylab), yvar, ylab)
  )
  
  # Add study labels if supplied
  if (!is.null(study_var)) {
    
    pos <- rep(4, length(x))
    pos[which.max(x)] <- 2
    
    text(
      x,
      y,
      labels = data[[study_var]],
      pos = pos,
      cex = 0.5
    )
  }
  
  x_pred <- seq(
    min(x, na.rm = TRUE),
    max(x, na.rm = TRUE),
    length.out = 100
  )
  
  # Linear fit
  pred_lin <- predict(
    linear_model,
    newmods = matrix(x_pred, ncol = 1)
  )
  
  lines(
    x_pred,
    pred_lin$pred,
    col = linear_col,
    lwd = 2
  )
  
  # Optional quadratic fit
  if (!is.null(quadratic_model)) {
    
    pred_quad <- predict(
      quadratic_model,
      newmods = cbind(x_pred, x_pred^2)
    )
    
    lines(
      x_pred,
      pred_quad$pred,
      col = quadratic_col,
      lwd = 2
    )
    
    legend(
      "topleft",
      legend = c("Linear", "Quadratic"),
      col = c(linear_col, quadratic_col),
      lwd = 2,
      bty = "n"
    )
    
  } else {
    
    legend(
      "topleft",
      legend = "Linear",
      col = linear_col,
      lwd = 2,
      bty = "n"
    )
    
  }
}