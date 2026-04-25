#' Compute Ordinal Expected Scores and DIF Effect Size (Cohen's h)
#'
#' Calculates expected scores for an ordinal item and DIF effect size
#' using Cohen's h metric.
#'
#' @param f Numeric: latent trait value
#' @param alpha Numeric vector: threshold parameters
#' @param beta Numeric: slope parameter
#'
#' @details
#' This function computes expected scores for ordinal response models
#' assuming thresholds at -Inf and +Inf at the boundaries.
#'
#' @return List or numeric value depending on usage
#' @export
#'
grm_expected_score <- function(f, alpha, beta) {
   # ordinal response model assumes a0=-Inf, ak+1=+Inf
   alpha <- c(-Inf, alpha, Inf)
   k <- length(alpha)
   score <- 0
   for (j in 2:k) {  # Start from 2 to account for -Inf
      prob_j <- stats::pnorm(alpha[j] - beta * f) - stats::pnorm(alpha[j - 1] - beta * f)
      score <- score + (j - 2) * prob_j  # (j - 2) to adjust for the starting index
   }
   score <- score / (k - 2)
   return(score)
}
