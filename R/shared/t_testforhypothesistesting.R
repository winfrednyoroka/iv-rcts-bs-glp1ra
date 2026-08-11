##################################################
# A function for comparing means at baseline----
#################################################
compare_baseline <- function(
    data,
    mean_int,
    mean_con,
    sd_int,
    sd_con,
    n_int,
    n_con,
    label_col = "comparison_label"
) {
  
  data |> 
    mutate(
      
      se_diff = sqrt(
        (.data[[sd_int]]^2 / .data[[n_int]]) +
          (.data[[sd_con]]^2 / .data[[n_con]])
      ),
      
      t = (
        .data[[mean_int]] -
          .data[[mean_con]]
      ) / se_diff,
      
      df = (
        (
          (.data[[sd_int]]^2 / .data[[n_int]]) +
            (.data[[sd_con]]^2 / .data[[n_con]])
        )^2
      ) /
        (
          ((.data[[sd_int]]^2 / .data[[n_int]])^2) /
            (.data[[n_int]] - 1) +
            ((.data[[sd_con]]^2 / .data[[n_con]])^2) /
            (.data[[n_con]] - 1)
        ),
      
      p_value = 2 * pt(
        -abs(t),
        df = df
      ),
      
      mean_difference =
        .data[[mean_int]] -
        .data[[mean_con]]
      
    ) |> 
    select(
      all_of(label_col),
      mean_difference,
      t,
      df,
      p_value
    )
}