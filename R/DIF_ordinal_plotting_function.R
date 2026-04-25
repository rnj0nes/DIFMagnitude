#' Plot Ordinal Expected Score Functions and DIF
#'
#' Creates a plot comparing the expected score functions for an ordinal item
#' across two groups, with shaded area between them.
#'
#' @param a1 Numeric vector of threshold parameters for group 1
#' @param b1 Numeric slope for group 1
#' @param a2 Numeric vector of threshold parameters for group 2
#' @param b2 Numeric slope for group 2
#' @param group1 Character string: label for group 1 (default: "Group 1")
#' @param group2 Character string: label for group 2 (default: "Group 2")
#' @param item Character string: item label for plot title (default: "Expected Score Functions")
#' @param f_range Numeric vector: range of latent trait values (default: -4 to 4)
#'
#' @return Invisibly NULL; creates a plot as side effect
#' @export
#'
DIF_ordinal_plotting_function <- function(a1, b1, a2, b2, 
                                          group1 = "Group 1", 
                                          group2 = "Group 2", 
                                          item = "Expected Score Functions", 
                                          f_range = seq(-4, 4, by = 0.01)) {
   # Define the expected score function for a given set of parameters
   expected_score <- function(f, alpha, beta) {
      # ordinal response model assumes a0=-Inf, ak+1=+Inf
      alpha <- c(-Inf, alpha, Inf)
      k <- length(alpha)
      score <- 0
      for (j in 2:k) {  # Start from 2 to account for -Inf
         prob_j <- stats::pnorm(alpha[j] - beta * f) - stats::pnorm(alpha[j - 1] - beta * f)
         score <- score + (j - 2) * prob_j  # (j - 2) to adjust for the starting index
      }
      score <- score / (k - 2)
      return(score)
   }
   
   # Define colors
   custom_blue <- grDevices::rgb(78, 54, 41, maxColorValue = 255)
   custom_red <- grDevices::rgb(237, 28, 36, maxColorValue = 255)
   less_intense_red <- grDevices::rgb(237, 28, 36, alpha = 64, maxColorValue = 255) # 50% transparency
   
   # Calculate expected scores for both sets of parameters
   es1_values <- sapply(f_range, function(f) expected_score(f, a1, b1))
   es2_values <- sapply(f_range, function(f) expected_score(f, a2, b2))
   
   # Plot the expected score functions
      graphics::plot(f_range, es1_values, type = "l", col = custom_blue, lwd = 2, ylim = c(0, 1),
               xlab = "latent trait", ylab = "E(u)/(number of levels)", main = item, yaxt = "n")
      graphics::axis(2, at = c(0, 0.25, 0.5, 0.75, 1.0), las = 1)  # Add custom y-axis ticks
      graphics::lines(f_range, es2_values, col = custom_red, lwd = 2)
   
   # Add labels
   graphics::legend("topleft", legend = c(group1, group2), col = c(custom_blue, custom_red), lwd = 2)
   
   # Fill the area between the curves
   graphics::polygon(c(f_range, rev(f_range)), c(es1_values, rev(es2_values)), col = less_intense_red, border = NA)
   
   # Re-draw the lines to ensure they are on top of the filled area
   graphics::lines(f_range, es1_values, col = custom_blue, lwd = 2)
   graphics::lines(f_range, es2_values, col = custom_red, lwd = 2)
}
