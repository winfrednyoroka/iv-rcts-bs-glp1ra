wald_ratio <- function(data,
                       beta_y,
                       se_y,
                       beta_x,
                       se_x) {
  
  by <- data[[beta_y]]
  sy <- data[[se_y]]
  bx <- data[[beta_x]]
  sx <- data[[se_x]]
  
  data$WR <- by / bx
  
  data$WR_SE <- sqrt(
    sy^2 / bx^2 +
      (by^2 * sx^2) / bx^4
  )
  
  data$WR_Lower95CI <- data$WR - 1.96 * data$WR_SE
  data$WR_Upper95CI <- data$WR + 1.96 * data$WR_SE
  
  data
}
