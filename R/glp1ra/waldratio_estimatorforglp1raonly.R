wald_ratio <- function(data,
                       beta_y,
                       se_y,
                       beta_x,
                       se_x,
                       suffix = "") {
  
  by <- data[[beta_y]]
  sy <- data[[se_y]]
  bx <- data[[beta_x]]
  sx <- data[[se_x]]
  
  data[[paste0("WR", suffix)]] <- by / bx
  
  data[[paste0("WR_SE", suffix)]] <- sqrt(
    sy^2 / bx^2 +
      (by^2 * sx^2) / bx^4
  )
  
  data[[paste0("WR_LCI", suffix)]] <-
    data[[paste0("WR", suffix)]] -
    1.96 * data[[paste0("WR_SE", suffix)]]
  
  data[[paste0("WR_UCI", suffix)]] <-
    data[[paste0("WR", suffix)]] +
    1.96 * data[[paste0("WR_SE", suffix)]]
  
  data
}