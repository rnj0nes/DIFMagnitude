# A function to compute the weighted and unsigned area (UA, comparable to Nye
# and Drasgow's dMACS statistic). If a covariance matrix for item parameters
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

genUA_dMACS <- function(params, musigma, sd = NULL, cov_matrix = NULL) {
   # Extract parameters
   a1 <- params[1]
   b1 <- params[2]
   a2 <- params[3]
   b2 <- params[4]
   mu <- musigma[1]
   sigma <- musigma[2]
   
   # Define the squared difference function
   f <- function(x) {
      f_x <- ((a1 - a2) + (b1 - b2) * x)^2  # Squared differences
      phi_x <- dnorm(x, mean = mu, sd = sigma)  # Latent trait density
      f_x * phi_x  # Weighted squared difference
   }
   
   # Compute standard errors if covariance matrix is provided
   if (!is.null(cov_matrix)) {
      # Compute the gradient of the function with respect to params
      dMACS_gradient <- numDeriv::grad(function(p) {
         a1 <- p[1]
         b1 <- p[2]
         a2 <- p[3]
         b2 <- p[4]
         integrate(function(x) {
            f_x <- ((a1 - a2) + (b1 - b2) * x)^2  # Squared differences
            phi_x <- dnorm(x, mean = mu, sd = sigma)  # Latent trait density
            f_x * phi_x
         }, lower = -4, upper = 4)$value
      }, params)
      
      # Delta method approximation of the variances
      dMACS_variance <- t(dMACS_gradient) %*% cov_matrix %*% dMACS_gradient
      # Standard errors
      dMACS_std_error <- sqrt(dMACS_variance)
   }
   
   # Integrate the squared differences to compute the area
   result <- integrate(f, lower = -4, upper = 4)
   
   # Take the square root of the integrated result (as in dMACS)
   sqrt_result <- sqrt(result$value)
   
   # Compute standardized dMACS
   dMACS <- if (!is.null(sd)) sqrt_result / sd else NA
   
   if (!is.null(cov_matrix)) {
      return(list(dMACS = sqrt_result, std_error = dMACS_std_error, std_dMACS = dMACS))
   } else {
      return(list(dMACS = sqrt_result, std_error = NA, std_dMACS = dMACS))
   }
}
