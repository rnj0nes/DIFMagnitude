test_that("signed_area is the directional integrated difference", {
   # μ = 0, σ = 1 — directional signed area = a2 - a1
   res <- DIFMagnitude::computeAreas(
      params  = c(a1 = 0.5, b1 = 1.0, a2 = 0.8, b2 = 1.0),
      musigma = c(mu = 0, sigma = 1)
   )
   expect_equal(res$signed_area, 0.3, tolerance = 1e-10)

   # μ = 1, slopes differ — directional signed area = (b2-b1)*mu + (a2-a1)
   res2 <- DIFMagnitude::computeAreas(
      params  = c(a1 = 0.5, b1 = 1.0, a2 = 0.8, b2 = 1.5),
      musigma = c(mu = 1, sigma = 1)
   )
   expect_equal(res2$signed_area, 0.5 * 1 + 0.3, tolerance = 1e-10)
})

test_that("signed_area sign matches intercept direction when mu = 0", {
   res <- DIFMagnitude::computeAreas(
      params  = c(a1 = 0.526, b1 = 0.916, a2 = 0.586, b2 = 1.012),
      musigma = c(mu = 0, sigma = 0.215)
   )
   expect_gt(res$signed_area, 0)
   expect_equal(res$signed_area, 0.586 - 0.526, tolerance = 1e-10)
})

test_that("signed_area_rms preserves legacy behavior", {
   res <- DIFMagnitude::computeAreas(
      params  = c(a1 = 0.5, b1 = 1.0, a2 = 0.8, b2 = 1.5),
      musigma = c(mu = 0, sigma = 1)
   )
   xc <- (0.8 - 0.5) / (1.0 - 1.5)
   diff_sq <- function(x) ((0.5 - 0.8) + (1.0 - 1.5) * x)^2 * stats::dnorm(x, 0, 1)
   above <- sqrt(stats::integrate(diff_sq, lower = xc, upper = Inf)$value)
   below <- sqrt(stats::integrate(diff_sq, lower = -Inf, upper = xc)$value)
   expect_equal(res$signed_area_rms, above - below, tolerance = 1e-6)
})

test_that("unsigned_area is unchanged from prior definition", {
   res <- DIFMagnitude::computeAreas(
      params  = c(a1 = 0.5, b1 = 1.0, a2 = 0.8, b2 = 1.5),
      musigma = c(mu = 0, sigma = 1)
   )
   diff_sq <- function(x) ((0.5 - 0.8) + (1.0 - 1.5) * x)^2 * stats::dnorm(x, 0, 1)
   expected <- sqrt(stats::integrate(diff_sq, lower = -Inf, upper = Inf)$value)
   expect_equal(res$unsigned_area, expected, tolerance = 1e-6)
})

test_that("standardization divides by sd", {
   res <- DIFMagnitude::computeAreas(
      params  = c(a1 = 0.5, b1 = 1.0, a2 = 0.8, b2 = 1.0),
      musigma = c(mu = 0, sigma = 1),
      sd = 0.25
   )
   expect_equal(res$std_signed_area, res$signed_area / 0.25, tolerance = 1e-12)
   expect_equal(res$std_unsigned_area, res$unsigned_area / 0.25, tolerance = 1e-12)
})

test_that("closed-form signed_se equals delta-method with gradient (-1,-mu,1,mu)", {
   cov4 <- diag(c(0.01, 0.02, 0.01, 0.02))
   mu <- 0.3
   res <- DIFMagnitude::computeAreas(
      params     = c(a1 = 0.5, b1 = 1.0, a2 = 0.8, b2 = 1.2),
      musigma    = c(mu = mu, sigma = 1),
      cov_matrix = cov4
   )
   g <- c(-1, -mu, 1, mu)
   expected_var <- as.numeric(t(g) %*% cov4 %*% g)
   expect_equal(res$signed_se^2, expected_var, tolerance = 1e-10)
})

test_that("alias compute_areas() matches computeAreas()", {
   p  <- c(0.5, 1.0, 0.8, 1.2)
   ms <- c(0, 1)
   r1 <- DIFMagnitude::computeAreas(params = p, musigma = ms)
   r2 <- DIFMagnitude::compute_areas(params = p, musigma = ms)
   expect_equal(r1, r2)
})

test_that("parallel slopes return NA for legacy RMS signed area but a finite directional signed_area", {
   res <- DIFMagnitude::computeAreas(
      params  = c(a1 = 0.5, b1 = 1.0, a2 = 0.8, b2 = 1.0),
      musigma = c(mu = 0, sigma = 1)
   )
   expect_true(is.na(res$signed_area_rms))
   expect_false(is.na(res$signed_area))
   expect_equal(res$signed_area, 0.3, tolerance = 1e-10)
})
