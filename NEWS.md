# DIFMagnitude 0.2.0

## Breaking changes

* `computeAreas()` (and its alias `compute_areas()`) now returns a
  **directional** `signed_area`, defined as
  \deqn{\int [\mathrm{ESF}_2 - \mathrm{ESF}_1]\,\phi\,d\theta = (b_2 - b_1)\mu + (a_2 - a_1).}
  Positive values indicate group 2 (focal) scores higher than group 1
  (reference) on average across the latent distribution.

* The previous "RMS-difference" signed area (sqrt of squared discrepancy
  above the crossover minus sqrt of squared discrepancy below) is preserved
  and now returned as `signed_area_rms`. Standardized and SE versions are
  returned as `std_signed_area_rms`, `signed_se_rms`, and `std_signed_se_rms`.

* `signed_se` is now computed in closed form via the gradient
  \eqn{(-1, -\mu, 1, \mu)} of the directional area; this is faster and
  numerically more stable than the previous numerical-gradient approach.

## Migration

If you previously consumed `result$signed_area` and want to keep the old
behavior, switch to `result$signed_area_rms`. If you want a directional
measure of which group scores higher (the more common interpretation), no
code change is required — `signed_area` now returns that quantity.
