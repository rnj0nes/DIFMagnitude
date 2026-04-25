#' Pull Item Parameters from Mplus H5 Output
#'
#' Extracts intercepts and slopes for an item from an H5RESULTS object
#' comparing two groups.
#'
#' @param h5 Character string: name of the h5 file
#' @param group1 Character string: reference group label from Mplus output
#' @param group2 Character string: focal group label from Mplus output
#' @param item Character string: item/variable name (continuous DV)
#' @param factorname Character string: name of the underlying latent variable
#'
#' @return A numeric vector c(a1, b1, a2, b2) where
#'   a1 = intercept for group1, b1 = slope for group1,
#'   a2 = intercept for group2, b2 = slope for group2
#' @export
#'
pull_ItemParameters <- function(h5, group1, group2, item, factorname) {
   if (!requireNamespace("mplush5", quietly = TRUE)) {
      stop("Package 'mplush5' is required for pull_ItemParameters().", call. = FALSE)
   }
   E <- mplush5::mplus.print.model.results(h5)
   a1 <- E$Estimate[
      grepl(item, E$Statement) &
         grepl(group1, E$Group) &
         grepl("Intercepts", E$Section)
   ]
   a2 <- E$Estimate[
      grepl(item, E$Statement) &
         grepl(group2, E$Group) &
         grepl("Intercepts", E$Section)
   ]
   b1 <- E$Estimate[
      grepl(item, E$Statement) &
         grepl(group1, E$Group) &
         grepl(paste(factorname, "BY"), E$Statement)
   ]
   b2 <- E$Estimate[
      grepl(item, E$Statement) &
         grepl(group2, E$Group) &
         grepl(paste(factorname, "BY"), E$Statement)
   ]
   c(a1, b1, a2, b2)
}
