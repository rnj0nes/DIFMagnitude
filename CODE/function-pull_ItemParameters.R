# Function to pull item parameters out of H5RESULTS object
#   for continuous dependent variables (pulls intercepts)
#
# Rich Jones
# Nov 25 2024
#
# Example usage:
# pull_itemParameters("mm06.h5","NHW","HISPSPAN","VDMDE5Z","GENMEM") 
#    where
#        "mm05.h5"    is the name of the h5 file 
#        "NHW"        is the label assigned (in the Mplus output) to 
#                     the reference group
#        "HISPAN"     is the label assigned (in the Mplus output) to
#                     the focal group 
#        "VDMDE5Z"    is the name of the CONTINUOUS dependent variable
#        "GENMEM"     is the label assigned to the underlying latent 
#                     variable
#
pull_itemParameters <- function(h5,group1,group2,item,factorname) {
   E <- mplus.print.model.results(h5) 
   a1 <- E |> filter(grepl(item, Statement) & grepl(group1,Group) & grepl("Intercepts",Section)) |> pull(Estimate)
   a2 <- E |> filter(grepl(item, Statement) & grepl(group2,Group) & grepl("Intercepts",Section)) |> pull(Estimate)
   b1 <- E |> filter(grepl(item, Statement) & grepl(group1,Group) & grepl(paste(factorname,"BY"),Statement)) |> pull(Estimate)
   b2 <- E |> filter(grepl(item, Statement) & grepl(group2,Group) & grepl(paste(factorname,"BY"),Statement)) |> pull(Estimate)
   c(a1,b1,a2,b2)
}
