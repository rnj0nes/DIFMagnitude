# Wrapper function to extract parameters and compute the weighted areas and 
# their standard errors, and plot the lines
# Example usage 
# Area_measures <- function(
#       h5,     # h5 object
#       item,   # item label (remember all caps)
#       group1, group2, # group labels (remember all caps), 
#                         group2 is the focal group, group1 
#                         is the reference group
#       factorname, # name of factor (remember all caps)
#       cov_matrix = NULL, 
#       group_df, # data frame with raw data
#       group_var) { # variable that holds group labels in raw data
# Area_measures("mm04.h5","VDMRE2Z","HISPENG","HISPSPAN","GENMEM",cov_matrix = NULL,source,"group3")

library(dplyr)
library(numDeriv)

Area_measures <- function(
         data,   # data frame with raw data, used in pooled_sd
         h5,     # h5 object
         item,   # item label (remember all caps)
         group1, group2, # group labels (remember all caps), 
         # group2 is the focal group, group1 is the reference group
         factorname, # name of latent variable (remember all caps)
         cov_matrix = NULL, 
         group_var, # variable that holds group information
         sdis = NULL ) { # if you'd like to use your own SD
                         # otherwise the default is the pooled sd over ALL groups
   params <- pull_itemParameters(h5,group1,group2,item,factorname) 
   musigma <- pull_muSigma(h5,group2,factorname)
   if (!is.null(sdis)) {
      pooled_sd_val <- sdis
   } else {
      pooled_sd_value <- data |> pooled_sd(group_var, item)
   }
   results <- computeAreas(params, musigma, sd = pooled_sd_value, cov_matrix = cov_matrix)
   print(areas)
   data |>  plot_lines(params, group1, group2, item)
   # Print the results
   #cat("Signed area:", SA[[1]], "\n")
   #cat("Unsigned area:", UA[[1]], "\n")
   #cat("Standardized signed area :", SA[[3]], "\n")
   #cat("Standardized unsigned area:", UA[[3]], "\n")
   #if (!is.null(cov_matrix)) {
   #   cat("Signed area standard error:", SA[[2]], "\n")
   #   cat("Unsigned area standard error:", UA[[2]], "\n")
   #}
   # Return the results as a list
   #results <- list(
   #    SA = SA[[1]],
   #    UA = UA[[1]],
   #    dSA = SA[[3]],
   #    dUA = UA[[3]]
   #)
   #if (!is.null(cov_matrix)) {
   #    results$SA_std_error <- SA[[2]]
   #    results$UA_std_error <- UA[[3]]
   #}
   return(results)
}
