###############################################################
# Script for visualising BMI vs BP at baseline vs followup----
##############################################################
###############################################################################
# Function: plot_bmi_bp_trajectory()
#
# Description:
# Creates trajectory plots showing changes in BMI (x-axis) and an outcome
# measure (y-axis, e.g. SBP, DBP) across follow-up time points for each
# intervention arm within a study comparison.
#
# Each point represents the mean BMI and mean outcome at a given follow-up
# month. Horizontal and vertical confidence intervals are displayed for BMI
# and the outcome respectively. Points belonging to the same arm are connected
# with arrows indicating the direction of change over time.
#
# Features:
# - Faceted plots by comparison/study.
# - Optional reference lines for BMI and outcome thresholds.
# - Automatic shape assignment for follow-up months.
# - Consistent shape mapping across studies.
# - Transparent confidence intervals.
# - Optional fixed axis limits across all facets and exported plots.
# - Optional export of individual study-level plots.
#
# Arguments:
#
# data
#   Data frame containing summary statistics for BMI and outcome measures.
#
# x_mean
#   Column containing mean BMI values.
#
# x_lower
#   Column containing lower confidence interval limits for BMI.
#
# x_upper
#   Column containing upper confidence interval limits for BMI.
#
# y_mean
#   Column containing mean outcome values (e.g. mean_SBP, mean_DBP).
#
# y_lower
#   Column containing lower confidence interval limits for the outcome.
#
# y_upper
#   Column containing upper confidence interval limits for the outcome.
#
# facet_var
#   Variable used for faceting the plot (typically comparison_id).
#
# group_var
#   Variable defining trajectories to be connected by lines/arrows
#   (typically intervention arm).
#
# colour_var
#   Variable used to colour trajectories and points.
#
# shape_var
#   Variable defining plotting symbols, typically follow-up month.
#
# point_size
#   Size of plotted points.
#
# bmi_ref
#   BMI reference value used for the vertical dashed reference line.
#   Default = 25.
#
# bp_ref
#   Outcome reference value used for the horizontal dashed reference line.
#   For example:
#     SBP = 120
#     DBP = 80
#
# scales
#   Facet scaling option passed to facet_wrap().
#   Options include:
#     "fixed" (default)
#     "free"
#     "free_x"
#     "free_y"
#
# ci_alpha
#   Transparency applied to horizontal and vertical confidence intervals.
#
# x_label
#   Label for the x-axis.
#
# y_label
#   Label for the y-axis.
#
# shape_label
#   Title for the shape legend.
#
# save_individual
#   Logical indicating whether individual study plots should be exported.
#
# output_dir
#   Directory where individual plots are saved.
#
# consistent_limits
#   Logical indicating whether all plots should use common x- and y-axis
#   limits derived from the entire dataset.
#
# width
#   Width (in inches) of exported figures.
#
# height
#   Height (in inches) of exported figures.
#
# Value:
# Returns a ggplot object. If save_individual = TRUE, separate PNG files are
# additionally saved for each comparison level.
#
# Example:
#
# p <- plot_bmi_bp_trajectory(
#   data = bmi_sbp_plot,
#   y_mean = mean_SBP,
#   y_lower = lowerbound_SBP,
#   y_upper = upperbound_SBP,
#   y_label = "Mean SBP (mmHg)",
#   bp_ref = 120,
#   scales = "free"
# )
#
# p
#
# Example: DBP
#
# p <- plot_bmi_bp_trajectory(
#   data = bmi_dbp_plot,
#   y_mean = mean_DBP,
#   y_lower = lowerbound_DBP,
#   y_upper = upperbound_DBP,
#   y_label = "Mean DBP (mmHg)",
#   bp_ref = 80
# )
#
###############################################################################

plot_bmi_bp_trajectory <- function(
    data,
    x_mean = mean_BMI,
    x_lower = lowerbound_BMI,
    x_upper = upperbound_BMI,
    y_mean = mean_SBP,
    y_lower = lowerbound_SBP,
    y_upper = upperbound_SBP,
    facet_var = comparison_id,
    group_var = arm_name,
    colour_var = treatment_group,
    shape_var = month_f,
    point_size = 2,
    bmi_ref = 25,
    bp_ref = 120,
    scales = "fixed",
    ci_alpha = 0.35,
    x_label = expression("BMI (kg/m"^2*")"),
    y_label = NULL,
    shape_label = "Months",
    save_individual = FALSE,
    output_dir = "figures",
    consistent_limits = TRUE,
    width = 8,
    height = 6
) {
  
  facet_var  <- enquo(facet_var)
  group_var  <- enquo(group_var)
  colour_var <- enquo(colour_var)
  shape_var  <- enquo(shape_var)
  
  ########################################################
  # Create a GLOBAL shape map from the full dataset
  ########################################################
  
  available_shapes <- c(
    16, 17, 15, 18, 5,
    0, 1, 8, 7, 13,
    2, 9, 10, 11, 12,
    14, 3, 4, 6
  )
  
  month_values <- data |>
    dplyr::pull(!!shape_var) |>
    as.character() |>
    unique()
  
  month_values <- month_values[order(as.numeric(month_values))]
  
  if (length(month_values) > length(available_shapes)) {
    stop("More unique months than available plotting symbols.")
  }
  
  shape_map <- setNames(
    available_shapes[seq_along(month_values)],
    month_values
  )
  
  #########################################################
  # Optional global axis limits
  ########################################################
  
  if (consistent_limits) {
    
    global_x <- range(
      c(
        dplyr::pull(data, {{ x_lower }}),
        dplyr::pull(data, {{ x_upper }})
      ),
      na.rm = TRUE
    )
    
    global_y <- range(
      c(
        dplyr::pull(data, {{ y_lower }}),
        dplyr::pull(data, {{ y_upper }})
      ),
      na.rm = TRUE
    )
    
  }
  
  ########################################################
  # Build Plot
  ########################################################
  
  p <- ggplot(
    data,
    aes(
      x = {{ x_mean }},
      y = {{ y_mean }},
      colour = !!colour_var,
      group = !!group_var
    )
  ) +
    
    geom_vline(
      xintercept = bmi_ref,
      linetype = "dashed",
      colour = "grey50"
    ) +
    
    geom_hline(
      yintercept = bp_ref,
      linetype = "dashed",
      colour = "grey50"
    ) +
    
    geom_path(
      linewidth = 0.8,
      alpha = 0.8,
      arrow = arrow(
        length = unit(0.08, "cm"),
        type = "closed"
      )
    ) +
    
    geom_errorbar(
      aes(
        ymin = {{ y_lower }},
        ymax = {{ y_upper }}
      ),
      width = 0,
      alpha = ci_alpha,
      linewidth = 0.5
    ) +
    
    geom_errorbarh(
      aes(
        xmin = {{ x_lower }},
        xmax = {{ x_upper }}
      ),
      height = 0,
      alpha = ci_alpha,
      linewidth = 0.5
    ) +
    
    geom_point(
      aes(shape = !!shape_var),
      size = point_size,
      stroke = 0.8
    ) +
    
    facet_wrap(
      vars(!!facet_var),
      scales = scales
    ) +
    
    scale_shape_manual(
      values = shape_map,
      drop = TRUE
    ) +
    
    labs(
      x = x_label,
      y = y_label,
      colour = NULL,
      shape = shape_label
    ) +
    
    theme_bw() +
    
    theme(
      legend.position = "bottom",
      strip.text = element_text(
        size = 7,
        lineheight = 0.8
      )
    )
  
  if (consistent_limits) {
    p <- p +
      coord_cartesian(
        xlim = global_x,
        ylim = global_y
      )
  }
  
  ########################################################
  # Save individual plots
  ########################################################
  
  if (save_individual) {
    
    dir.create(
      output_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    comparisons <- unique(
      dplyr::pull(data, !!facet_var)
    )
    
    for (i in comparisons) {
      
      tmp <- data |>
        filter((!!facet_var) == i)
      
      pp <- ggplot(
        tmp,
        aes(
          x = {{ x_mean }},
          y = {{ y_mean }},
          colour = !!colour_var,
          group = !!group_var
        )
      ) +
        
        geom_vline(
          xintercept = bmi_ref,
          linetype = "dashed",
          colour = "grey50"
        ) +
        
        geom_hline(
          yintercept = bp_ref,
          linetype = "dashed",
          colour = "grey50"
        ) +
        
        geom_path(
          linewidth = 0.8,
          alpha = 0.8,
          arrow = arrow(
            length = unit(0.08, "cm"),
            type = "closed"
          )
        ) +
        
        geom_errorbar(
          aes(
            ymin = {{ y_lower }},
            ymax = {{ y_upper }}
          ),
          width = 0,
          alpha = ci_alpha
        ) +
        
        geom_errorbarh(
          aes(
            xmin = {{ x_lower }},
            xmax = {{ x_upper }}
          ),
          height = 0,
          alpha = ci_alpha
        ) +
        
        geom_point(
          aes(shape = !!shape_var),
          size = point_size
        ) +
        
        scale_shape_manual(
          values = shape_map,
          drop = TRUE
        ) +
        
        labs(
          x = x_label,
          y = y_label,
          colour = NULL,
          shape = shape_label
        ) +
        
        theme_bw() +
        theme(
          legend.position = "bottom"
        )
      
      if (consistent_limits) {
        pp <- pp +
          coord_cartesian(
            xlim = global_x,
            ylim = global_y
          )
      }
      
      safe_name <- gsub(
        "[^A-Za-z0-9]+",
        "_",
        i
      )
      
      ggsave(
        filename = file.path(
          output_dir,
          paste0(safe_name, ".png")
        ),
        plot = pp,
        width = width,
        height = height,
        dpi = 300
      )
    }
  }
  
  return(p)
}