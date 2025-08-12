# Function to pull mean and variance of latent trait for focal group out of
# an H5RESULTS object. Note that this will pull the RESIDUAL VARIANCE (and 
# not the variance) if the model has covariates. So, some other strategy will
# be needed to get the variance of the latent trait in the focal group if the 
# model includes covariates (e.g., TECH4). The residual variance should NOT be
# be used to weight the areas.
#
# Rich Jones
# Nov 25 2024
#
# Example usage:
# pull_muSigma("mm06.h5","HISPSPAN","GENMEM")
#
pull_muSigma <- function(h5,group2,factorname) {
   E <- mplus.print.model.results(h5) 
   mu <- E |> filter(grepl(factorname, Statement) & grepl(group2,Group) & grepl("Means",Section)) |> pull(Estimate)
   sigma <- E |> filter(grepl(factorname, Statement) & grepl(group2,Group) & grepl("Variances",Section)) |> pull(Estimate)
   c(mu,sigma)
}
