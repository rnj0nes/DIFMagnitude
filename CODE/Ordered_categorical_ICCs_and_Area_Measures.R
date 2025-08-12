# Rich Jones
# 2024-09-07
#
# This R file contains functions that are useful for 
# (a) displaying and (b) expressing the magnitude of
# differences in item response functions (in the form of
# expected item score functions) in the context of an
# item-response theory based analysis of differential
# item functioning.
#
# Intended for output from Mplus/WLSMV/theta: The expected
# input for the functions are "a" parameters and "b" 
# parameters (from the probit partial credit or 2PN 
# framework if the item is scored in only 2 categories) 
# where the analytic framework is using the latent 
# response variable approach (e.g., Mplus categorical 
# variables). One set of "a" and "b" parameters is 
# expected for each group. Treat group 1 as the 
# reference group and group 2 as the focal group.
#
# Note this will also work for Mplus/Bayes and
# Mplus MLR/link=probit.
#
# The function also expects a mean and variance for the
# focal group (mu2 and sigmasquared2) 
#
# Functions in this R file will:
#  1. compute the expected score function over
#     a range of theta scores given a and b 
#     parameters (ExpectedScore) 
#  2. Compute the signed and unsigned area
#     measures scaled to Cohen's h (AreaMeasures)
#  3. Plot the epected response curves.
#
# Function: "expectedScore"
# Compute the expected score function for a given set of 
# parameters. Input should be f, alpha, and beta to be 
# defined something like:
#
# fis <- seq(-4, 4, by = 0.01)  # Quadrature points
# a1 <- c(-2.135, 0.635, 2.135)
# b1 <- 0.711
#
# use like this:
#
# score1 <- sapply(fis, function(f) expectedScore(f, a1, b1))
#
# * Note this expected score will always range between 
#   0 and 1, regardless of how many response categories 
#   the item has. This is done in the 
#
#                  "score <- score / (k-2)"
#
#   line if you want to remove that. It is a device we 
#   take advantage of when computing the area measures.

expectedScore <- function(f, alpha, beta) {
   # ordinal response model assumes a0=-Inf, ak+1=+Inf
   alpha <- c(-Inf, alpha, Inf)
   k <- length(alpha)
   score <- 0
   for (j in 2:k) {  # Start from 2 to account for -Inf
      # The pnorm let's you know we are expecting 
      # parameters from the ordered probit regression
      # model. For example Mplus WLSMV theta.
      prob_j <- pnorm(alpha[j] - beta * f) - pnorm(alpha[j - 1] - beta * f)
      score <- score + (j - 2) * prob_j  # (j - 2) to adjust for the starting index
   }
   score <- score / (k - 2)
   return(score)
}

# Function: areaMeasures 
# The area measure is the weighted sum of Cohen's h effect
# size statistic over the range of ability (defined by the
# user, in fis) and weighted according to the distribution
# of ability in the focal group (as implied by the mean and 
# standard deviation of ability in the focal group provided
# by the user in `mu2` and `sigmasquared2`).
#
# Cohen's h effect size statistic is for the difference in
# proportions. It is computed as 
#
#    h = 2*asin(sqrt(p2)) - 2*asin(sqrt(p1))
#
# and is interpreted like Cohen's d statistic where .2, .5 
# and .8 demarcate small, medium and large effects (Cohen, 1988).
# In our framework, we have proportions for specific quadrature
# points on the ability dimension, which are expected proportions 
# of the total possible number of points on the item at that 
# ability level for members of the reference group (score1)
# and the focal group (score2). 
#
# We compute the weighted -- to the focal group ability 
# distribution -- mean h across the quadrature points, and 
# label this "SAh". This is analogous to the signed area 
# between the ICCs. 
#
# We also take the absolute value of h, and compute the weighted
# mean of that. We label this the unsigned area ("UAh").
#
# Both SAh and UAh are returned.
#
#
#  use like this:
#
# areaMeasures(score1, score2, mu2, sigmasquared2, fis)
# $SAh
# [1] 0.1202699
# 
# $UAh
# [1] 0.3825437
# where score1 and score2 come from the ExpectedScore function given
# a1, b1, and a2, b2, respectively, and mu2 is the mean of the latent
# trait in the focal group and sigmasquared2 is the variance of the
# latent trait in the focal group.
#
library(stats)
areaMeasures <- function(score1, score2, mu2, sigmasquared2, fis) {
   # Calculate the weights based on the normal distribution
   weight <- dnorm(fis, mean = mu2, sd = sqrt(sigmasquared2))  # Weights

   # calculate difference in scores
   SAp <- sum((score2-score1) * weight) / sum(weight)
   UAp <- sum(abs(score2-score1) * weight) / sum(weight)
   
   # Calculate h
   h <- 2 * (asin(sqrt(score2)) - asin(sqrt(score1)))
   # Calculate the absolute value of h
   absh <- abs(h)
   
   
   # Calculate the weighted mean of h
   SAh <- sum(h * weight) / sum(weight)
   
   # Calculate the weighted mean of abs(h)
   UAh <- sum(absh * weight) / sum(weight)
   
   # Return the calculated values as a list
   return(list(SAh = SAh, UAh = UAh, SAp = SAp, UAp = UAp))
}
#
# Function: plotERF
# This is a simple wrapper for plot that 
# will make an expected item response function (ERF)
# plot for the two groups.
#
#  example use:
#
#  plotERF(fis, score1, score2)
plotERF <- function(fis, score1, score2, 
                    xlab = "Latent Trait (f)", 
                    ylab = "Expected Score/Total Item Score", 
                    legend_labels = c("Focal", "Reference")) {
   # Plot score2 over fis as a simple line plot
   plot(fis, score2, type = "l", col = "red", ylim = c(0, 1), 
        xlab = xlab, ylab = ylab)
   
   # Add score1 to the plot in red
   lines(fis, score1, col = "blue")
   
   # Add a legend to differentiate the two lines
   legend("bottomright", legend = legend_labels, col = c("red", "blue"), lty = 1)
}
# plotERF(fis, score1, score2)

# Running through the three functions
# Define input parameters
# These are results from Mplus or other software
### This example is a toy example where the
### thresholds and loadings are different
### but the expected response functions are
### nearly identical. It's a mind-bender
### see below for something more expected
### a1 <- c(-1, 0, 1)
### b1 <- 0.85
### a2 <- c(-0.5, 0, 0.5)
### b2 <- 0.7
a1 <- c(-0.25, .25, 0.75)
b1 <- 0.6
a2 <- c(-0.5, 0, 0.5)
b2 <- 0.8
mu2 <- -1
sigmasquared2 <- 1
# Set quadrature points, the range of theta you'd like to 
# see the plots and define the ERFs. From -4 to +4 is 
# typical, so you might not want to edit the line below
fis <- seq(-4, 4, by = 0.01)  # Quadrature points
#
# Use the functions
score1 <- sapply(fis, function(f) expectedScore(f, a1, b1))
score2 <- sapply(fis, function(f) expectedScore(f, a2, b2))
areaMeasures(score1, score2, mu2, sigmasquared2, fis)
plotERF(fis, score1, score2)
