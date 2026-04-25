#' Compute Pooled Standard Deviation Across Groups
#'
#' Calculates the pooled standard deviation for a variable across k groups
#' using the formula: sqrt(sum((n_i - 1) * sd_i^2) / (N - k))
#'
#' @param group_df A data frame containing the grouping variable and item
#' @param groupvar Character string: name of the grouping variable
#' @param item Character string: name of the target variable
#'
#' @return The pooled standard deviation (numeric)
#' @export
#'
#' @examples
#' # pooled_sd(data, "group_var", "score_var")
#'
pooled_sd <- function(group_df, groupvar, item) {
   # force varnames and items to uppercase
   # because Mplus is case insensitive and this is written to
   # work with H5RESULTS
   item <- toupper(item)
   groupvar <- toupper(groupvar)
   DF <- group_df |> dplyr::rename_all(toupper)
   DF <- DF |> dplyr::select(dplyr::all_of(c(groupvar, item)))
   
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
      subset_data <- DF |> dplyr::filter(DF[[groupvar]] == level)
      n <- sum(!is.na(subset_data[[item]])) # Count non-missing values
      sd_val <- stats::sd(subset_data[[item]], na.rm = TRUE)
      sum_sq_dev <- sum_sq_dev + (n - 1) * sd_val^2
      total_n <- total_n + n
   }
   
   # Calculate the pooled standard deviation
   pooled_sd_val <- sqrt(sum_sq_dev / (total_n - k))
   print(paste("Pooled sd over", k, "levels of", groupvar, " = ", pooled_sd_val))
   return(pooled_sd_val)
}
