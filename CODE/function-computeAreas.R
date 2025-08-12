computeAreas <- function(params, musigma, sd = NULL, cov_matrix = NULL) {
   # Extract parameters
   a1 <- params[1]
   b1 <- params[2]
   a2 <- params[3]
   b2 <- params[4]
   mu <- musigma[1]
   sigma <- musigma[2]
   
   # Define the squared difference function
   squared_f <- function(x) {
      diff <- (a1 - a2) + (b1 - b2) * x
      phi_x <- dnorm(x, mean = mu, sd = sigma)  # Latent trait density
      (diff^2) * phi_x
   }
   
   # Solve for crossover point
   if (b1 != b2) {
      xc <- (a2 - a1) / (b1 - b2)
   } else {
      xc <- NULL  # Parallel lines; no crossover point
   }
   
   # Compute unsigned area (full integral)
   total_result <- integrate(squared_f, lower = -Inf, upper = Inf)
   unsigned_area <- sqrt(total_result$value)
   
   # Compute signed area if slopes are different
   if (!is.null(xc)) {
      # Above region
      above_result <- integrate(squared_f, lower = xc, upper = Inf)
      D_above <- sqrt(above_result$value)
      
      # Below region
      below_result <- integrate(squared_f, lower = -Inf, upper = xc)
      D_below <- sqrt(below_result$value)
      
      # Signed area
      signed_area <- D_above - D_below
   } else {
      signed_area <- NA
   }
   
   # Compute standardized areas if pooled SD is supplied
   if (!is.null(sd)) {
      std_unsigned_area <- unsigned_area / sd
      std_signed_area <- if (!is.null(xc)) signed_area / sd else NA
   } else {
      std_unsigned_area <- NA
      std_signed_area <- NA
   }
   
   # Compute standard errors if covariance matrix is supplied
   if (!is.null(cov_matrix)) {
      # Gradient for unsigned area
      unsigned_gradient <- numDeriv::grad(function(p) {
         a1 <- p[1]
         b1 <- p[2]
         a2 <- p[3]
         b2 <- p[4]
         integrate(squared_f, lower = -Inf, upper = Inf)$value
      }, params)
      
      unsigned_variance <- t(unsigned_gradient) %*% cov_matrix %*% unsigned_gradient
      unsigned_se <- sqrt(unsigned_variance)
      
      # Gradient for signed area (only if slopes are different)
      if (!is.null(xc)) {
         signed_gradient <- numDeriv::grad(function(p) {
            a1 <- p[1]
            b1 <- p[2]
            a2 <- p[3]
            b2 <- p[4]
            if (b1 != b2) {
               xc <- (a2 - a1) / (b1 - b2)
               above <- integrate(squared_f, lower = xc, upper = Inf)$value
               below <- integrate(squared_f, lower = -Inf, upper = xc)$value
               sqrt(above) - sqrt(below)
            } else {
               NA
            }
         }, params)
         
         signed_variance <- t(signed_gradient) %*% cov_matrix %*% signed_gradient
         signed_se <- sqrt(signed_variance)
      } else {
         signed_se <- NA
      }
      
      # Standardized SEs
      if (!is.null(sd)) {
         std_unsigned_se <- unsigned_se / sd
         std_signed_se <- if (!is.null(xc)) signed_se / sd else NA
      } else {
         std_unsigned_se <- NA
         std_signed_se <- NA
      }
   } else {
      unsigned_se <- NA
      signed_se <- NA
      std_unsigned_se <- NA
      std_signed_se <- NA
   }
   
   # Return all results as a list
   return(list(
      unsigned_area = unsigned_area,
      signed_area = signed_area,
      std_unsigned_area = std_unsigned_area,
      std_signed_area = std_signed_area,
      unsigned_se = unsigned_se,
      signed_se = signed_se,
      std_unsigned_se = std_unsigned_se,
      std_signed_se = std_signed_se
   ))
}
