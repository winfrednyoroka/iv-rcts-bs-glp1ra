plot_forest <- function(
    model,
    xlab = "Wald Ratio"
) {
  
  metafor::forest(
    model,
    xlab = xlab,
    cex = 0.8,
    mlab = paste0(
      "Random-effects model (I² = ",
      round(model$I2, 1),
      "%)"
    )
  )
  
}