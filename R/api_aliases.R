#' Compute DIF Magnitude Areas (Preferred Name)
#'
#' Preferred snake_case alias for computeAreas().
#'
#' @inheritParams computeAreas
#' @return A list containing unsigned_area, signed_area, and their standardized and SE versions
#' @export
compute_areas <- function(params, musigma, sd = NULL, cov_matrix = NULL) {
   computeAreas(params = params, musigma = musigma, sd = sd, cov_matrix = cov_matrix)
}

#' Pull Item Parameters from Mplus H5 Output (Preferred Name)
#'
#' Preferred snake_case alias for pull_ItemParameters().
#'
#' @inheritParams pull_ItemParameters
#' @return A numeric vector c(a1, b1, a2, b2)
#' @export
pull_item_parameters <- function(h5, group1, group2, item, factorname) {
   pull_ItemParameters(
      h5 = h5,
      group1 = group1,
      group2 = group2,
      item = item,
      factorname = factorname
   )
}

#' Pull Mean and Variance from Mplus H5 Output (Preferred Name)
#'
#' Preferred snake_case alias for pull_muSigma().
#'
#' @inheritParams pull_muSigma
#' @return A numeric vector c(mu, sigma)
#' @export
pull_mu_sigma <- function(h5, group2, factorname) {
   pull_muSigma(h5 = h5, group2 = group2, factorname = factorname)
}

#' Plot Ordinal Expected Score Functions (Preferred Name)
#'
#' Preferred snake_case alias for DIF_ordinal_plotting_function().
#'
#' @inheritParams DIF_ordinal_plotting_function
#' @return Invisibly NULL; creates a plot as side effect
#' @export
plot_ordinal_expected_scores <- function(a1, b1, a2, b2,
                                         group1 = "Group 1",
                                         group2 = "Group 2",
                                         item = "Expected Score Functions",
                                         f_range = seq(-4, 4, by = 0.01)) {
   DIF_ordinal_plotting_function(
      a1 = a1,
      b1 = b1,
      a2 = a2,
      b2 = b2,
      group1 = group1,
      group2 = group2,
      item = item,
      f_range = f_range
   )
}

#' Register Mplus knitr Engine (Preferred Name)
#'
#' Preferred snake_case alias for mplus_engine().
#'
#' @return Used for side effects; registers knitr engine
#' @export
register_mplus_engine <- function() {
   mplus_engine()
}