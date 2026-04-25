# DIFMagnitude

DIFMagnitude provides functions for computing differential item functioning (DIF)
magnitude indices from expected response functions, with emphasis on weighted
signed and unsigned area metrics for continuous and ordinal outcomes.

## Installation

Install dependencies:

```r
install.packages(c("devtools", "dplyr", "numDeriv"))
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("rhdf5")
devtools::install_github("dougtommet/mplush5")
```

Install from GitHub:

```r
devtools::install_github("rnj0nes/DIFMagnitude")
```

Install from local source:

```r
devtools::install(".")
```

## Preferred API (snake_case)

- compute_areas
- pooled_sd
- pull_item_parameters
- pull_mu_sigma
- plot_lines
- plot_ordinal_expected_scores
- grm_expected_score
- register_mplus_engine

## Backward Compatibility

Legacy function names are still available for compatibility:

- computeAreas
- pull_ItemParameters
- pull_muSigma
- DIF_ordinal_plotting_function
- mplus_engine

## Minimal Continuous-Item Workflow

```r
h5_file <- "DATA/mm06.h5"
group1 <- "NHW"
group2 <- "HISPSPAN"
item <- "VDMRE2Z"
factor_name <- "GENMEM"

params <- pull_item_parameters(h5_file, group1, group2, item, factor_name)
musigma <- pull_mu_sigma(h5_file, group2, factor_name)

# group_df must contain the grouping variable and item
psd <- pooled_sd(group_df, "group3", item)

res <- compute_areas(params, musigma, sd = psd, cov_matrix = NULL)
print(res)

plot_lines(group_df, params, group1, group2, item)
```

## Notes

- H5RESULTS helper functions depend on mplush5.
- For models with covariates, latent variance extraction may require TECH4-based handling.
- Mplus label matching is case-sensitive in practice when filtering parameter tables.
