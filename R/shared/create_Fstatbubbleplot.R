# Create a bubble plot of F-statistics by study and follow-up duration.
#
# Each point represents a study-timepoint comparison. Point size is
# proportional to the F-statistic, allowing visual identification of
# studies with larger between-group differences.
#
# Arguments:
#   data          Data frame containing study-level F-statistics.
#   study_col     Column containing study identifiers.
#   duration_col  Column containing follow-up duration values.
#   fstat_col     Column containing F-statistics.
#   xlab          Xaxis title
#   ylab          Yaxis title
# Returns:
#   A ggplot object.

plot_fstat <- function(
    data,
    study_col = "StudyID",
    duration_col = "StudyDuration",
    fstat_col = "f_stat",
    xlab = "Follow-up duration (months)",
    ylab = "Author (Year)"
) {
  
  ggplot(
    data,
    aes(
      x = .data[[duration_col]],
      y = forcats::fct_inorder(.data[[study_col]]),
      size = .data[[fstat_col]]
    )
  ) +
    geom_point(
      alpha = 0.5,
      shape = 16
    ) +
    scale_size_continuous(
      name = "F-statistic",
      range = c(2, 12)
    ) +
    scale_x_continuous(
      breaks = sort(unique(data[[duration_col]]))
    ) +
    labs(
      x = xlab,
      y = ylab
    ) +
    theme_classic()
}