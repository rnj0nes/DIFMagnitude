# A function to compute the weighted and unsigned area (UA, comparable to Raju's
# NCDIF statistic for ordinal tiems). If a covariance matrix for item parameters
# is provided, the standard error is computed.
# 
# Rich Jones
# Nov 25 2024
#
# Example usage:
# params <- pull_itemParameters("mm06.h5","NHW","HISPSPAN","VDMDE5Z","GENMEM") 
# musigma <- pull_muSigma("mm06.h5","HISPSPAN","GENMEM")
# genUA(params, musigma, sd=.15)
#

library(dplyr)
library(numDeriv)

genUA <- function(params, musigma, sd = NULL, cov_matrix = NULL) {
   a1 <- params[1]
   b1 <- params[2]
   a2 <- params[3]
   b2 <- params[4]
   mu <- musigma[1]
   sigma <- musigma[2]
   f <- function(x) {
      f_x <- abs((a1 - a2) + (b1 - b2) * x)
      phi_x <- dnorm(x, mean = mu, sd = sigma)
      f_x * phi_x
   }
   # Compute standard errors if cov_matrix is provided
   if (!is.null(cov_matrix)) {
      # Compute the gradient of the function with respect to params
      UA_gradient <- numDeriv::grad(function(p) {
         a1 <- p[1]
         b1 <- p[2]
         a2 <- p[3]
         b2 <- p[4]
         integrate(function(x) {
            f_x <- abs((a1 - a2) + (b1 - b2) * x)
            phi_x <- dnorm(x, mean = mu, sd = sigma)
            f_x * phi_x
         }, lower = -4, upper = 4)$value
      }, params)
      # Delta method approximation of the variances
      UA_variance <- t(UA_gradient) %*% cov_matrix %*% UA_gradient
      # Standard errors
      UA_std_error <- sqrt(UA_variance)
   }
   result <- integrate(f, lower = -4, upper = 4)
   # Compute standardized UA
   dUA <- if (!is.null(sd)) result$value / sd else NA
   if (!is.null(cov_matrix)) {
      return(list(UA = result$value, std_error = UA_std_error, dUA = dUA))
   } else {
      return(list(UA = result$value, std_error = NA, dUA = dUA))
   }
}
