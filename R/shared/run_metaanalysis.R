run_rem_meta <- function(
    data,
    yi_col = "WR",
    sei_col = "WR_SE"
) {
  
  metafor::rma(
    yi = data[[yi_col]],
    sei = data[[sei_col]],
    method = "REML",
    slab = data$study_id
  )
  
}