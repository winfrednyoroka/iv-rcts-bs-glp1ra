# calc_wr <- function(data,
#                     exposure_outcome,
#                     exposure_times,
#                     outcome_outcome,
#                     outcome_times) {
#   
#   data |>
#     filter(
#       (Outcome == exposure_outcome &
#          post_time_months %in% exposure_times) |
#         (Outcome == outcome_outcome &
#            post_time_months %in% outcome_times)
#     ) |>
#     pivot_wider(
#       id_cols = c(
#         comparison_id,
#         arm_name_Intervention,
#         arm_name_Control
#       ),
#       names_from = c(Outcome, post_time_months),
#       values_from = c(beta, se)
#     )
# }

calc_wr <- function(data,
                    exposure_outcome,
                    outcome_outcome,
                    times) {
  
  data |>
    filter(
      Outcome %in% c(exposure_outcome, outcome_outcome),
      post_time_months %in% times
    ) |>
    pivot_wider(
      id_cols = comparison_id,
      names_from = c(Outcome, post_time_months),
      values_from = c(beta, se)
    )
}