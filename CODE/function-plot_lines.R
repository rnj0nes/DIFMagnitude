# Function to plot the lines 
#
# Rich Jones
# Nov 25 2024
#
# Example usage:
# params <- pull_itemParameters("mm06.h5","NHW","HISPSPAN","VDMRE2Z","GENMEM") 
# plot_lines(source, params,"NHW","HISPSPAN","VDMRE2Z")
#
# params is c(a1,b1,a2,b2) as per pull_itemParameters
# group1 is group1 label 
# group2 is group2 label
# item is item label
# group_df is a dataframe that has item in it (for getting ranges)
plot_lines <- function(data, params, group1, group2, item) {
   # Extract parameters
   a1 <- params[1]
   b1 <- params[2]
   a2 <- params[3]
   b2 <- params[4]
   # Convert column names to uppercase
   DF <- data %>% rename_all(toupper)
   # Calculate ymin and ymax based on the specified item and going back to 
   # the source data
   ymin <- min(DF[[toupper(item)]], na.rm = TRUE)
   ymax <- max(DF[[toupper(item)]], na.rm = TRUE)
   # Define the custom colors
   custom_red <- rgb(237, 28, 36, maxColorValue = 255)
   custom_blue <- rgb(78, 54, 41, maxColorValue = 255)
   less_intense_red <- rgb(237, 28, 36, alpha = 64, maxColorValue = 255) # 50% transparency
   # Generate x values for the area plot
   x_vals <- seq(-4, 4, length.out = 1000)
   # Calculate y values for both lines
   y_vals1 <- a1 + b1 * x_vals
   y_vals2 <- a2 + b2 * x_vals
   # Create a new plot
   plot(NULL, xlim = c(-4, 4), ylim = c(ymin, ymax), 
        xlab = "Latent trait", ylab = "E(y)", main = toupper(item))
   # Plot the first line
   curve(a1 + b1 * x, from = -4, to = 4, col = custom_blue, lwd = 2, add = TRUE)
   # Plot the second line with the custom RGB color
   curve(a2 + b2 * x, from = -4, to = 4, col = custom_red, lwd = 2, add = TRUE)
   # Plot the area between the two lines
   polygon(c(x_vals, rev(x_vals)), c(y_vals1, rev(y_vals2)), col = less_intense_red, border = NA)
   # Add a legend with group labels
   legend("topleft", legend = c(group1, group2), col = c(custom_blue, custom_red), lwd = 2)
}
