# Function to compute pooled standard deviation over k groups
#
# Rich Jones
# Nov 25 2024
#
# Example usage:
# pooled_sd(source_data_frame, "name of grouping variable", "name of target variable")
#
library(dplyr)
pooled_sd <- function(group_df, groupvar, item) {
   # Load necessary package
   library(dplyr)
   # force varnames and items to uppercase
   # because Mplus is case insensitivive and this is written to
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
