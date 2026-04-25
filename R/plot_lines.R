#' Plot Response Functions for Two Groups
#'
#' Creates a plot comparing the expected response functions for two groups,
#' displaying parameters as lines with shaded area between them.
#'
#' @param data A data frame containing the item variable for range calculations
#' @param params A numeric vector c(a1, b1, a2, b2) of intercepts and slopes
#' @param group1 Character string: label for reference group
#' @param group2 Character string: label for focal group
#' @param item Character string: name of the item variable
#'
#' @return Invisibly NULL; creates a plot as side effect
#' @export
#'
plot_lines <- function(data, params, group1, group2, item) {
   # Extract parameters
   a1 <- params[1]
   b1 <- params[2]
   a2 <- params[3]
   b2 <- params[4]
   
   # Convert column names to uppercase
   DF <- data |> dplyr::rename_all(toupper)
   
   # Calculate ymin and ymax based on the specified item and going back to 
   # the source data
   ymin <- min(DF[[toupper(item)]], na.rm = TRUE)
   ymax <- max(DF[[toupper(item)]], na.rm = TRUE)
   
   # Define the custom colors
   custom_red <- grDevices::rgb(237, 28, 36, maxColorValue = 255)
   custom_blue <- grDevices::rgb(78, 54, 41, maxColorValue = 255)
   less_intense_red <- grDevices::rgb(237, 28, 36, alpha = 64, maxColorValue = 255) # 50% transparency
   
   # Generate x values for the area plot
   x_vals <- seq(-4, 4, length.out = 1000)
   
   # Calculate y values for both lines
   y_vals1 <- a1 + b1 * x_vals
   y_vals2 <- a2 + b2 * x_vals
   
   # Create a new plot
   graphics::plot(NULL, xlim = c(-4, 4), ylim = c(ymin, ymax),
                  xlab = "Latent trait", ylab = "E(y)", main = toupper(item))
   
   # Plot both lines
   graphics::lines(x_vals, y_vals1, col = custom_blue, lwd = 2)
   graphics::lines(x_vals, y_vals2, col = custom_red, lwd = 2)
   
   # Plot the area between the two lines
   graphics::polygon(c(x_vals, rev(x_vals)), c(y_vals1, rev(y_vals2)), col = less_intense_red, border = NA)
   graphics::lines(x_vals, y_vals1, col = custom_blue, lwd = 2)
   graphics::lines(x_vals, y_vals2, col = custom_red, lwd = 2)
   
   # Add a legend with group labels
   graphics::legend("topleft", legend = c(group1, group2), col = c(custom_blue, custom_red), lwd = 2)
}
