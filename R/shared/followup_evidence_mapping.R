###############################################################
# Function: plot_study_outcome_coverage()
#
# Description:
# Creates a follow-up outcome matrix for longitudinal studies.
# Each study is displayed on the y-axis and assessment time
# points are displayed on the x-axis.
#
# Outcome symbols:
#   - BMI : grey-filled square
#   - HTN : large open circle
#   - SBP : smaller open circle
#   - DBP : central black dot
#
# Expected input data:
# The input dataframe should contain the following variables:
#
#   Study       : study identifier (factor or character)
#   Month_plot  : x-axis plotting coordinate
#   BMI         : logical indicator for BMI outcome
#   HTN         : logical indicator for hypertension outcome
#   SBP         : logical indicator for systolic blood pressure
#   DBP         : logical indicator for diastolic blood pressure
#
# Arguments:
#   data                Input dataframe.
#   study_order         Optional dataframe containing desired
#                       study ordering.
#   breaks              X-axis break locations.
#   labels              Labels corresponding to x-axis breaks.
#   xlab                X-axis title.
#   ylab                Y-axis title.
#   xlab_size           X-axis title font size.
#   ylab_size           Y-axis title font size.
#   axis_text_x_angle   Rotation angle of x-axis labels.
#   axis_text_x_size    Font size of x-axis labels.
#   axis_text_y_size    Font size of y-axis labels.
#   bmi_fill            Fill colour used for BMI squares.
#   legend              Logical indicating whether the custom
#                       outcome legend should be displayed.
#
# Returns:
#   A ggplot object that can be further modified using
#   additional ggplot2 layers and themes.
#
# Notes:
#   - The function assumes that follow-up times have already
#     been transformed into Month_plot values.
#   - A compressed time scale can be created by mapping
#     120 months to an alternative plotting position
#     (e.g. Month_plot = 70) while retaining the axis label
#     "120".
#
# Example:
#
# long <- long |> 
#   mutate(
#     Month_plot = ifelse(Month == 120, 70, Month)
#   )
#
# p <- plot_study_outcome_coverage(long)
# p
###############################################################

plot_study_outcome_coverage <- function(
    data,
    study_order = NULL,
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
    bmi_fill = "grey90",
    legend = TRUE
) {
  
  require(ggplot2)
  require(dplyr)
  
  if (is.null(study_order)) {
    study_levels <- levels(data$Study)
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
  
  # BMI background square
  
  p <- p +
    geom_point(
      data = filter(data, BMI),
      aes(shape = "BMI"),
      size = 10,
      fill = bmi_fill,
      colour = bmi_fill
    )
  
  # HTN outer circle
  
  p <- p +
    geom_point(
      data = filter(data, HTN),
      aes(shape = "HTN"),
      size = 7,
      stroke = 1.2,
      fill = NA
    )
  
  # SBP inner circle
  
  p <- p +
    geom_point(
      data = filter(data, SBP),
      aes(shape = "SBP"),
      size = 4,
      stroke = 1.2,
      fill = NA
    )
  
  # DBP centre point
  
  p <- p +
    geom_point(
      data = filter(data, DBP),
      aes(shape = "DBP"),
      size = 1.5
    )
  
  p <- p +
    scale_x_continuous(
      breaks = breaks,
      labels = labels,
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
      
      axis.text.x.top = element_blank(),
      axis.ticks.x.top = element_blank(),
      
      axis.text.x = element_text(
        size = axis_text_x_size,
        angle = axis_text_x_angle,
        hjust = 0.5
      ),
      
      axis.text.y = element_text(
        size = axis_text_y_size,
        colour = "black"
      ),
      
      axis.title.x = element_text(
        size = xlab_size,
        colour = "black",
        face = "bold"
      ),
      
      axis.title.y = element_text(
        size = ylab_size,
        colour = "black",
        face = "bold"
      )
    )
  
  if (legend) {
    
    p <- p +
      scale_shape_manual(
        name = "Outcomes",
        values = c(
          BMI = 22,
          DBP = 16,
          HTN = 21,
          SBP = 21
          
        ),
        labels = c(
          BMI = "Body mass index",
          DBP = "Diastolic blood pressure",
          HTN = "Hypertension",
          SBP = "Systolic blood pressure"
          
        ),
        drop = TRUE
      ) +
      guides(
        shape = guide_legend(
          override.aes = list(
            size = c(8, 2, 5, 3),
            fill = c(
              bmi_fill,
              "black",
              NA,
              NA
              
            ),
            colour = c(
              bmi_fill,
              "black",
              "black",
              "black"
            ),
            stroke = c(
              0,
              0,
              1.2,
              1.2
            )
          )
        )
      ) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(
          face = "bold"
        ),
        legend.text = element_text(
          size = 11
        )
      )
    
  } else {
    
    p <- p +
      theme(
        legend.position = "none"
      )
    
  }
  
  return(p)
}

