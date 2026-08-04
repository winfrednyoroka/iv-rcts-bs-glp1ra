###############################################################
# Function: plot_study_outcome_coverage()
#
# Description:
# Creates a longitudinal study outcome coverage matrix.
# Each study is displayed on the y-axis and assessment time
# points are displayed on the x-axis.
#
# The function is outcome-driven: it automatically detects and
# plots only those outcome variables available in the input
# dataframe. This allows the same function to be used for
# different combinations of outcomes (e.g. BMI only, BMI + SBP +
# DBP, or BMI + HTN + SBP + DBP).
#
# Outcome symbols:
#   - BMI : grey-filled square
#   - HTN : large open circle
#   - SBP : smaller open circle
#   - DBP : central black dot
#
# Expected input data:
#
# The input dataframe should contain:
#
#   Study       : study identifier (factor or character)
#   Month_plot  : x-axis plotting coordinate
#
# Optional outcome variables:
#
#   BMI         : logical indicator for body mass index outcome
#   HTN         : logical indicator for hypertension outcome
#   SBP         : logical indicator for systolic blood pressure
#   DBP         : logical indicator for diastolic blood pressure
#
# At least one outcome variable must be present.
#
# Arguments:
#
#   data:
#       Input dataframe containing study identifiers,
#       follow-up time coordinates, and outcome indicators.
#
#   study_order:
#       Optional dataframe specifying desired ordering of studies.
#       If NULL, the order of Study levels in the data is used.
#
#   outcomes:
#       Character vector specifying outcome variables to consider.
#       Default:
#       c("BMI", "HTN", "SBP", "DBP")
#
#       Only outcomes present in the dataframe are plotted.
#
#   breaks:
#       Numeric vector specifying x-axis break positions.
#
#   labels:
#       Character vector specifying labels corresponding to
#       x-axis break positions.
#
#   xlab:
#       X-axis title.
#
#   ylab:
#       Y-axis title.
#
#   xlab_size:
#       Font size of x-axis title.
#
#   ylab_size:
#       Font size of y-axis title.
#
#   axis_text_x_angle:
#       Rotation angle of x-axis tick labels.
#
#   axis_text_x_size:
#       Font size of x-axis tick labels.
#
#   axis_text_y_size:
#       Font size of y-axis study labels.
#
#   bmi_fill:
#       Fill colour for BMI square symbol.
#
#   legend:
#       Logical indicating whether the outcome legend should be
#       displayed.
#
# Returns:
#
#   A ggplot2 object that can be further modified using
#   additional ggplot2 layers, scales, or themes.
#
# Notes:
#
#   - Outcome variables should be coded as logical indicators
#     (TRUE/FALSE).
#
#   - Follow-up times should be transformed before plotting
#     using Month_plot.
#
#   - A compressed time scale can be created by mapping long
#     follow-up periods to alternative plotting coordinates
#     while retaining the original labels.
#
# Example:
#
#   long <- long |>
#     mutate(
#       Month_plot = ifelse(Month == 120, 70, Month)
#     )
#
#   # Plot all available outcomes
#   p <- plot_study_outcome_coverage(long)
#
#   # Plot only BMI and blood pressure outcomes
#   p <- plot_study_outcome_coverage(
#          long,
#          outcomes = c("BMI", "SBP", "DBP")
#        )
#
###############################################################
plot_study_outcome_coverage <- function(
  data,
  study_order = NULL,
  outcomes = c("BMI", "HTN", "SBP", "DBP"),
  breaks = c(3, 6, 9, 12, 18, 24, 36, 48, 60, 70),
  labels = c(
    "3m", "6m", "9m", "12m",
    "18m", "24m", "36m",
    "48m", "60m", "120m"
  ),
  xlab = "Follow-up time (months)",
  ylab = "Author (Year)",
  xlab_size = 13,
  ylab_size = 13,
  axis_text_x_angle = 0,
  axis_text_x_size = 12,
  axis_text_y_size = 12,
  hjust_size = 0.5,
  bmi_fill = "grey90",
  legend = TRUE
) {
  require(ggplot2)
  require(dplyr)

  # Keep only outcomes available in the dataset
  outcomes_present <- intersect(
    outcomes,
    names(data)
  )

  if (length(outcomes_present) == 0) {
    stop("No outcome variables found in data.")
  }

  # Study ordering

  if (is.null(study_order)) {
    study_levels <- levels(factor(data$Study))
  } else {
    study_levels <- rev(study_order$Study)
  }

  p <- ggplot(
    data,
    aes(
      x = Month_plot,
      y = Study
    )
  ) +
    scale_y_discrete(
      limits = study_levels
    )


  # ---- BMI square ----

  if ("BMI" %in% outcomes_present) {
    p <- p +
      geom_point(
        data = filter(data, BMI),
        aes(shape = "BMI"),
        size = 8,
        fill = bmi_fill,
        colour = bmi_fill
      )
  }


  # ---- HTN outer circle ----

  if ("HTN" %in% outcomes_present) {
    p <- p +
      geom_point(
        data = filter(data, HTN),
        aes(shape = "HTN"),
        size = 6,
        stroke = 1.2,
        fill = NA
      )
  }


  # ---- SBP circle ----

  if ("SBP" %in% outcomes_present) {
    p <- p +
      geom_point(
        data = filter(data, SBP),
        aes(shape = "SBP"),
        size = 3,
        stroke = 1.2,
        fill = NA
      )
  }


  # ---- DBP dot ----

  if ("DBP" %in% outcomes_present) {
    p <- p +
      geom_point(
        data = filter(data, DBP),
        aes(shape = "DBP"),
        size = 1,
        colour = "black"
      )
  }


  # ---- Base theme ----

  p <- p +
    scale_x_continuous(
      breaks = breaks,
      labels = labels,
      limits = c(0, max(breaks)),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      x = xlab,
      y = ylab
    ) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      axis.text.x = element_text(
        size = axis_text_x_size,
        angle = axis_text_x_angle,
        hjust = hjust_size
      ),
      axis.text.y = element_text(
        size = axis_text_y_size
      ),
      axis.title.x = element_text(
        size = xlab_size,
        face = "bold"
      ),
      axis.title.y = element_text(
        size = ylab_size,
        face = "bold"
      )
    )


  # ---- Dynamic legend ----

  if (legend) {
    shape_values <- c(
      BMI = 22,
      HTN = 21,
      SBP = 21,
      DBP = 16
    )

    labels_values <- c(
      BMI = "Body mass index",
      HTN = "Hypertension",
      SBP = "Systolic blood pressure",
      DBP = "Diastolic blood pressure"
    )

    p <- p +
      scale_shape_manual(
        name = "Outcomes",
        values = shape_values[outcomes_present],
        labels = labels_values[outcomes_present],
        drop = TRUE
      ) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 11)
      )
  } else {
    p <- p +
      theme(
        legend.position = "none"
      )
  }

  return(p)
}
