# Load necessary library
library(stats)

# Define the plotting function
plot_expected_scores <- function(a1, b1, a2, b2, group1 = "Group 1", group2 = "Group 2", item = "Expected Score Functions", f_range = seq(-4, 4, by = 0.01)) {
   # Define the expected score function for a given set of parameters
   expected_score <- function(f, alpha, beta) {
      # ordinal response model assumes a0=-Inf, ak+1=+Inf
      alpha <- c(-Inf, alpha, Inf)
      k <- length(alpha)
      score <- 0
      for (j in 2:k) {  # Start from 2 to account for -Inf
         prob_j <- pnorm(alpha[j] - beta * f) - pnorm(alpha[j - 1] - beta * f)
         score <- score + (j - 2) * prob_j  # (j - 2) to adjust for the starting index
      }
      score <- score / (k - 2)
      return(score)
   }
   
   # Define colors
   custom_blue <- rgb(78, 54, 41, maxColorValue = 255)
   custom_red <- rgb(237, 28, 36, maxColorValue = 255)
   less_intense_red <- rgb(237, 28, 36, alpha = 64, maxColorValue = 255) # 50% transparency
   
   # Calculate expected scores for both sets of parameters
   es1_values <- sapply(f_range, function(f) expected_score(f, a1, b1))
   es2_values <- sapply(f_range, function(f) expected_score(f, a2, b2))
   
   # Plot the expected score functions
   plot(f_range, es1_values, type = "l", col = custom_blue, lwd = 2, ylim = c(0,1),
        xlab = "latent trait", ylab = "E(u)/(number of levels)", main = item, yaxt = "n")
   axis(2, at = c(0, 0.25, 0.5, 0.75, 1.0), las=1)  # Add custom y-axis ticks
   lines(f_range, es2_values, col = custom_red, lwd = 2)
   
   # Add labels
   legend("topleft", legend = c(group1, group2), col = c(custom_blue, custom_red), lwd = 2)
   
   # Fill the area between the curves
   polygon(c(f_range, rev(f_range)), c(es1_values, rev(es2_values)), col = less_intense_red, border = NA)
   
   # Re-draw the lines to ensure they are on top of the filled area
   lines(f_range, es1_values, col = custom_blue, lwd = 2)
   lines(f_range, es2_values, col = custom_red, lwd = 2)
}

# Example code
a1 <- c(0.135, 0.635, 1.135)
b1 <- 0.711 
a2 <- c(-0.471, 0.029, 0.529)
b2 <- 0.991 
mu2 <- 0.011 
sigmasquared2 <- 1 
group1 <- "Women"
group2 <- "Men" 
item <- "Spell: girder"

plot_expected_scores(a1, b1, a2, b2, group1, group2, item)
