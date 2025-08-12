
devtools::install_github("dougtommet/mplush5")

library(dplyr)
library(numDeriv)
##############################################################################
# Function to compute pooled standard deviation over k groups
pooled_sd <- function(group_df, groupvar, item) {
   # Load necessary package
   library(dplyr)
   # force varnames and items to uppercase
   # because Mplus is case insensitive and this is written to
   # work with H5RESULTS
   item <- toupper(item)
   groupvar <- toupper(groupvar)
   DF <- group_df %>% rename_all(toupper)
   DF <- DF %>% select(all_of(c(groupvar, item))) 
   # Ensure the grouping variable is a factor
   DF[[groupvar]] <- as.factor(DF[[groupvar]])
   # Calculate the number of levels in the grouping variable
   levels <- levels(DF[[groupvar]])
   k <- length(levels)
   # Initialize variables to store sums of squared deviations and total sample size
   sum_sq_dev <- 0
   total_n <- 0
   # Loop over each level to calculate the sum of squared deviations and total sample size
   for (level in levels) {
      subset_data <- DF %>% filter(DF[[groupvar]] == level)
      n <- sum(!is.na(subset_data[[item]])) # Count non-missing values
      sd_val <- sd(subset_data[[item]], na.rm = TRUE)
      sum_sq_dev <- sum_sq_dev + (n - 1) * sd_val^2
      total_n <- total_n + n
   }
   # Calculate the pooled standard deviation
   pooled_sd_val <- sqrt(sum_sq_dev / (total_n - k))
   print(paste("Pooled sd over",k,"levels of",groupvar," = ",pooled_sd_val))
   return(pooled_sd_val)
}
# Example usage:
# pooled_sd(source, "group3", "vdmde5z")
##############################################################################
# Function to pull item parameters out of H5RESULTS object
# Note that the a's are intercepts and b's are slopes
# in y = a + bx form.
# Not to be confused with the roles of a's and b's in 
# the 2PL model f(u) = a*(θ-b) = -ba + aθ
pull_itemParameters <- function(h5,group1,group2,item,factorname) {
   E <- mplus.print.model.results(h5) 
   a1 <- E |> filter(grepl(item, Statement) & grepl(group1,Group) & grepl("Intercepts",Section)) |> pull(Estimate)
   a2 <- E |> filter(grepl(item, Statement) & grepl(group2,Group) & grepl("Intercepts",Section)) |> pull(Estimate)
   b1 <- E |> filter(grepl(item, Statement) & grepl(group1,Group) & grepl(paste(factorname,"BY"),Statement)) |> pull(Estimate)
   b2 <- E |> filter(grepl(item, Statement) & grepl(group2,Group) & grepl(paste(factorname,"BY"),Statement)) |> pull(Estimate)
   c(a1,b1,a2,b2)
}
# Example usage:
# pull_itemParameters("mm06.h5","NHW","HISPSPAN","VDMDE5Z","GENMEM") 
##############################################################################
# Function to pull mean and variance of latent trait for focal group out of
# an H5RESULTS object. Note that this will pull the RESIDUAL VARIANCE (and 
# not the variance) if the model has covariates. So, some other strategy will
# be needed to get the variance of the latent trait in the focal group if the 
# model includes covariates (e.g., TECH4). The residual variance should NOT be
# be used to weight the areas.
pull_muSigma <- function(h5,group2,factorname) {
   E <- mplus.print.model.results(h5) 
   mu <- E |> filter(grepl(factorname, Statement) & grepl(group2,Group) & grepl("Means",Section)) |> pull(Estimate)
   sigma <- E |> filter(grepl(factorname, Statement) & grepl(group2,Group) & grepl("Variances",Section)) |> pull(Estimate)
   c(mu,sigma)
}
# Example usage:
# pull_muSigma("mm06.h5","HISPSPAN","GENMEM")

##############################################################################
# Function to plot the lines 
# params is c(a1,b1,a2,b2) as per pull_itemParameters
# group1 is group1 label 
# group2 is group2 label
# item is item label
# group_df is a dataframe that has item in it (for getting ranges)
plot_lines <- function(params, group1, group2, item, group_df) {
   # Extract parameters
   a1 <- params[1]
   b1 <- params[2]
   a2 <- params[3]
   b2 <- params[4]
   # Convert column names to uppercase
   DF <- group_df %>% rename_all(toupper)
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
# Example usage:
# params <- pull_itemParameters("mm06.h5","NHW","HISPSPAN","VDMRE2Z","GENMEM") 
# plot_lines(params,"NHW","HISPSPAN","VDMRE2Z",source)
