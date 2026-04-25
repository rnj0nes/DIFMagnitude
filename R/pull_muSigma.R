#' Pull Mean and Variance from Mplus H5 Output
#'
#' Extracts the mean and variance (or residual variance) of the latent trait
#' for a focal group from an H5RESULTS object.
#'
#' Note: If the model includes covariates, this pulls the RESIDUAL VARIANCE,
#' not the total variance. For covariate models, use alternative strategies
#' (e.g., TECH4) to obtain the full variance.
#'
#' @param h5 Character string: name of the h5 file
#' @param group2 Character string: focal group label from Mplus output
#' @param factorname Character string: name of the latent variable
#'
#' @return A numeric vector c(mu, sigma) with mean and standard deviation
#' @export
#'
pull_muSigma <- function(h5, group2, factorname) {
   if (!requireNamespace("mplush5", quietly = TRUE)) {
      stop("Package 'mplush5' is required for pull_muSigma().", call. = FALSE)
   }
   E <- mplush5::mplus.print.model.results(h5)
   mu <- E$Estimate[
      grepl(factorname, E$Statement) &
         grepl(group2, E$Group) &
         grepl("Means", E$Section)
   ]
   sigma <- E$Estimate[
      grepl(factorname, E$Statement) &
         grepl(group2, E$Group) &
         grepl("Variances", E$Section)
   ]
   c(mu, sigma)
}
