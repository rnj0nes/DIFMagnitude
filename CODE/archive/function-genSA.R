# A function to compute the signed area (SA), weighted to the expected 
# (focal) group ability distributaion, and standard error if a 
# covariance matrix of parameters is provided. This statistic is 
# analogous to Raju's CDIF statistic for ordinal tiems. 
#
# Rich Jones
# Nov 25, 2024
#
# Example usage:
# params <- pull_itemParameters("mm06.h5","NHW","HISPSPAN","VDMDE5Z","GENMEM") 
# musigma <- pull_muSigma("mm06.h5","HISPSPAN","GENMEM")
# genSA(params, musigma)

library(dplyr)
library(numDeriv)

genSA <- function(params, musigma, sd = NULL, cov_matrix = NULL) {
   a1 <- params[1]
   b1 <- params[2]
   a2 <- params[3]
   b2 <- params[4]
   mu <- musigma[1]
   sigma <- musigma[2]
   f <- function(x) {
      f_x <- ((a1 - a2) + (b1 - b2) * x)
      phi_x <- dnorm(x, mean = mu, sd = sigma)
      f_x * phi_x
   }
   # Compute standard errors if cov_matrix is provided
   if (!is.null(cov_matrix)) {
      # Compute the gradient of the function with respect to params
      SA_gradient <- numDeriv::grad(function(p) {
         a1 <- p[1]
         b1 <- p[2]
         a2 <- p[3]
         b2 <- p[4]
         integrate(function(x) {
            f_x <- ((a1 - a2) + (b1 - b2) * x)
            phi_x <- dnorm(x, mean = mu, sd = sigma)
            f_x * phi_x
         }, lower = -4, upper = 4)$value
      }, params)
      # Delta method approximation of the variances
      SA_variance <- t(SA_gradient) %*% cov_matrix %*% SA_gradient
      # Standard errors
      SA_std_error <- sqrt(SA_variance)
   }
   result <- integrate(f, lower = -4, upper = 4)
   # Compute standardized SA
   dSA <- if (!is.null(sd)) result$value / sd else NA
   if (!is.null(cov_matrix)) {
      return(list(SA = result$value, std_error = SA_std_error, dSA = dSA))
   } else {
      return(list(SA = result$value, std_error = NA, dSA = dSA))
   }
}
