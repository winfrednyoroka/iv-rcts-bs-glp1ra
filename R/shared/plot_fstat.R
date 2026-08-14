# Plot fstats and oclor by instrument strength

plot_fstat_colored <- function(
    data,
    study_col = "StudyID",
    duration_col = "StudyDuration",
    fstat_col = "f_stat",
    f_threshold = 10,
    xlab = "Follow-up duration (months)",
    ylab = "Author (Year)"
) {
  
  ggplot(
    data,
    aes(
      x = .data[[duration_col]],
      y = forcats::fct_inorder(.data[[study_col]]),
      size = .data[[fstat_col]],
      colour = .data[[fstat_col]] < f_threshold
    )
  ) +
    geom_point(
      alpha = 1
    ) +
    scale_colour_manual(
      values = c(
        "TRUE" = "#D55E00",
        "FALSE" = "grey30"
      ),
      labels = c(
        "FALSE" = paste0("F ≥ ", f_threshold),
        "TRUE"  = paste0("F < ", f_threshold)
      ),
      name = "Instrument strength"
    ) +
    scale_size_continuous(
      name = "F-statistic",
      range = c(2, 8)
    ) +
    scale_x_continuous(
      breaks = sort(unique(data[[duration_col]]))
    ) +
    labs(
      x = xlab,
      y = ylab
    ) +
    theme_classic() +
    theme(
      legend.position = "right"
    )
  
}