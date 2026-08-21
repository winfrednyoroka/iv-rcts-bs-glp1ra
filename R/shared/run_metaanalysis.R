run_rem_meta <- function(
    data,
    yi_col = "WR",
    sei_col = "WR_SE",
    method_choice = "REML"
) {
  
  metafor::rma(
    yi = data[[yi_col]],
    sei = data[[sei_col]],
    method = method_choice,
    slab = data$study_id
  )
  
}