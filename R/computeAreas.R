#' Compute DIF Magnitude Areas
#'
#' Computes weighted-area effect-size measures comparing two linear expected
#' score functions \eqn{\mathrm{ESF}_g(\theta) = a_g + b_g\theta}, \eqn{g \in \{1, 2\}},
#' under a normal latent-trait density \eqn{\phi(\theta; \mu, \sigma)}.
#'
#' @param params A numeric vector \code{c(a1, b1, a2, b2)} where
#'   \code{a1, b1} are the intercept and slope of the reference group (group 1)
#'   and \code{a2, b2} are the intercept and slope of the focal group (group 2).
#' @param musigma A numeric vector \code{c(mu, sigma)} for the latent trait
#'   distribution used as the integration weight.
#' @param sd Optional numeric. Item-score SD used to standardize the area
#'   measures (Cohen's \eqn{d}-like).
#' @param cov_matrix Optional 4x4 numeric covariance matrix of \code{params}
#'   used to compute standard errors.
#'
#' @details
#' Two signed area measures are returned:
#'
#' \describe{
#'   \item{\code{signed_area} (directional, default)}{
#'     The integrated difference between the focal and reference ESFs:
#'     \deqn{\int [\mathrm{ESF}_2(\theta) - \mathrm{ESF}_1(\theta)] \, \phi(\theta;\mu,\sigma)\,d\theta = (b_2 - b_1)\mu + (a_2 - a_1).}
#'     Positive values indicate that group 2 (focal) scores higher than
#'     group 1 (reference) on average across the latent distribution.
#'   }
#'   \item{\code{signed_area_rms} (RMS-difference rule, legacy)}{
#'     \deqn{\sqrt{\int_{\theta > x_c}\!\! \mathrm{diff}^2\,\phi\,d\theta} - \sqrt{\int_{\theta < x_c}\!\! \mathrm{diff}^2\,\phi\,d\theta},}
#'     where \eqn{x_c = (a_2 - a_1)/(b_1 - b_2)} and \eqn{\mathrm{diff} = (a_1 - a_2) + (b_1 - b_2)\theta}.
#'     This is the value returned by \code{signed_area} in DIFMagnitude
#'     versions \eqn{<} 0.2.0; it has been retained for backward
#'     compatibility but is *not* a directional measure of which group scores
#'     higher and may disagree in sign with \code{signed_area} when the
#'     crossover lies in the tail of \eqn{\phi}.
#'   }
#' }
#'
#' \code{unsigned_area} is the RMS discrepancy
#' \eqn{\sqrt{\int (\mathrm{ESF}_1 - \mathrm{ESF}_2)^2 \phi\,d\theta}} and is
#' unchanged from prior versions.
#'
#' @return A named list with elements:
#'   \code{unsigned_area}, \code{signed_area},
#'   \code{std_unsigned_area}, \code{std_signed_area},
#'   \code{unsigned_se}, \code{signed_se},
#'   \code{std_unsigned_se}, \code{std_signed_se},
#'   \code{signed_area_rms}, \code{std_signed_area_rms},
#'   \code{signed_se_rms}, \code{std_signed_se_rms}.
#' @export
computeAreas <- function(params, musigma, sd = NULL, cov_matrix = NULL) {
   # Extract parameters (drop names so downstream scalars are unnamed)
   a1 <- unname(params[1])
   b1 <- unname(params[2])
   a2 <- unname(params[3])
   b2 <- unname(params[4])
   mu <- unname(musigma[1])
   sigma <- unname(musigma[2])

   # Squared difference, using diff = ESF_1 - ESF_2 (consistent with prior versions)
   squared_f <- function(x) {
      diff <- (a1 - a2) + (b1 - b2) * x
      phi_x <- stats::dnorm(x, mean = mu, sd = sigma)
      (diff^2) * phi_x
   }

   # Crossover point (NULL if slopes equal)
   xc <- if (b1 != b2) (a2 - a1) / (b1 - b2) else NULL

   # Unsigned area (full RMS integral) -- unchanged
   total_result <- stats::integrate(squared_f, lower = -Inf, upper = Inf)
   unsigned_area <- sqrt(total_result$value)

   # ---- Directional signed area (NEW DEFAULT) ------------------------------
   # ∫ [ESF_2 - ESF_1] φ dθ = (b2 - b1) * mu + (a2 - a1)
   signed_area <- (b2 - b1) * mu + (a2 - a1)

   # ---- Legacy RMS-difference signed area (preserved) ----------------------
   if (!is.null(xc)) {
      above_result <- stats::integrate(squared_f, lower = xc, upper = Inf)
      below_result <- stats::integrate(squared_f, lower = -Inf, upper = xc)
      signed_area_rms <- sqrt(above_result$value) - sqrt(below_result$value)
   } else {
      signed_area_rms <- NA_real_
   }

   # Standardized versions
   if (!is.null(sd)) {
      std_unsigned_area    <- unsigned_area    / sd
      std_signed_area      <- signed_area      / sd
      std_signed_area_rms  <- if (!is.null(xc)) signed_area_rms / sd else NA_real_
   } else {
      std_unsigned_area   <- NA_real_
      std_signed_area     <- NA_real_
      std_signed_area_rms <- NA_real_
   }

   # ---- Standard errors via delta method ----------------------------------
   if (!is.null(cov_matrix)) {
      # Unsigned: numerical gradient (unchanged)
      unsigned_gradient <- numDeriv::grad(function(p) {
         a1 <- p[1]; b1 <- p[2]; a2 <- p[3]; b2 <- p[4]
         sf <- function(x) {
            d <- (a1 - a2) + (b1 - b2) * x
            (d^2) * stats::dnorm(x, mean = mu, sd = sigma)
         }
         sqrt(stats::integrate(sf, lower = -Inf, upper = Inf)$value)
      }, params)
      unsigned_variance <- as.numeric(t(unsigned_gradient) %*% cov_matrix %*% unsigned_gradient)
      unsigned_se <- sqrt(unsigned_variance)

      # Directional signed: closed-form gradient = (-1, -mu, 1, mu)
      signed_gradient <- c(-1, -mu, 1, mu)
      signed_variance <- as.numeric(t(signed_gradient) %*% cov_matrix %*% signed_gradient)
      signed_se <- sqrt(signed_variance)

      # Legacy RMS signed: numerical gradient (only when slopes differ)
      if (!is.null(xc)) {
         signed_rms_gradient <- numDeriv::grad(function(p) {
            a1 <- p[1]; b1 <- p[2]; a2 <- p[3]; b2 <- p[4]
            if (b1 == b2) return(NA_real_)
            xc_ <- (a2 - a1) / (b1 - b2)
            sf <- function(x) {
               d <- (a1 - a2) + (b1 - b2) * x
               (d^2) * stats::dnorm(x, mean = mu, sd = sigma)
            }
            above <- stats::integrate(sf, lower = xc_, upper = Inf)$value
            below <- stats::integrate(sf, lower = -Inf, upper = xc_)$value
            sqrt(above) - sqrt(below)
         }, params)
         signed_rms_variance <- as.numeric(t(signed_rms_gradient) %*% cov_matrix %*% signed_rms_gradient)
         signed_se_rms <- sqrt(signed_rms_variance)
      } else {
         signed_se_rms <- NA_real_
      }

      # Standardized SEs
      if (!is.null(sd)) {
         std_unsigned_se   <- unsigned_se   / sd
         std_signed_se     <- signed_se     / sd
         std_signed_se_rms <- if (!is.null(xc)) signed_se_rms / sd else NA_real_
      } else {
         std_unsigned_se   <- NA_real_
         std_signed_se     <- NA_real_
         std_signed_se_rms <- NA_real_
      }
   } else {
      unsigned_se       <- NA_real_
      signed_se         <- NA_real_
      signed_se_rms     <- NA_real_
      std_unsigned_se   <- NA_real_
      std_signed_se     <- NA_real_
      std_signed_se_rms <- NA_real_
   }

   list(
      unsigned_area        = unsigned_area,
      signed_area          = signed_area,
      std_unsigned_area    = std_unsigned_area,
      std_signed_area      = std_signed_area,
      unsigned_se          = unsigned_se,
      signed_se            = signed_se,
      std_unsigned_se      = std_unsigned_se,
      std_signed_se        = std_signed_se,
      signed_area_rms      = signed_area_rms,
      std_signed_area_rms  = std_signed_area_rms,
      signed_se_rms        = signed_se_rms,
      std_signed_se_rms    = std_signed_se_rms
   )
}
