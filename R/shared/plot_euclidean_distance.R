################################################################################
# Script for visualising the Euclidean distance----
# Create a scatter plot of Euclidean distances by study/comparison label.
#
# The function plots Euclidean distance values on the x-axis and orders
# labels on the y-axis according to the distance values. This is useful
# for visually comparing the magnitude of distances across studies.
#
# Arguments:
#   data           Data frame containing the plotting variables.
#   x_var          Name of the column containing Euclidean distance values.
#                  Default: "euclidean_distance".
#   y_var          Name of the column containing labels. Default: "label".
#   point_size     Size of plotted points. Default: 5.
#   point_colour   Colour of plotted points. Default: "gray40".
#   xlabel         X-axis title.
#   ylabel         Y-axis title.
#
# Returns:
#   A ggplot object that can be printed, saved or further modified with
#   additional ggplot layers.
#
# Example:
#   p <- plot_euclidean_distance(comparison_data)
#   p
###############################################################################

plot_euclidean_distance <- function(data,
                                    x_var = "euclidean_distance",
                                    y_var = "label",
                                    point_size = 5,
                                    point_colour = "gray40",
                                    xlabel = "Euclidean Distance",
                                    ylabel = "Author (Year) (Comparisons)") {
  
  ggplot(
    data,
    aes(
      x = .data[[x_var]],
      y = forcats::fct_reorder(.data[[y_var]], .data[[x_var]])
    )
  ) +
    geom_point(
      size = point_size,
      colour = point_colour
    ) +
    labs(
      x = xlabel,
      y = ylabel
    ) +
    theme_classic()
}