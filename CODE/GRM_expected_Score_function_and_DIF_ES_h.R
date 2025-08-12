# Load necessary library
library(stats)

a1 <- c(0.135, 0.635, 1.135)
b1 <- 0.711
a2 <- c(-0.471, 0.635, 1.135)
b2 <- 0.991
mu2 <- 0.011
sigmasquared2 <- 1

# Define the expected score function for a given set of parameters
expected_score <- function(f, alpha, beta) {
   # ordinal response model assumes a0=-Inf, ak+1=+Inf
   alpha <- c(-Inf, alpha, Inf)
   k <- length(alpha)
   score <- 0
   for (j in 2:k) {  # Start from 2 to account for -Inf
      prob_j <- pnorm(alpha[j] - beta * f) - pnorm(alpha[j - 1] - beta * f)
      score <- score + (j - 2) * prob_j  # (j - 2) to adjust for the starting index
   }
   score <- score / (k - 2)
   return(score)
}

fis <- seq(-4, 4, by = 0.01)
score1 <- sapply(fis, function(f) expected_score(f, a1, b1))
score2 <- sapply(fis, function(f) expected_score(f, a2, b2))
h <- 2 * (asin(sqrt(score2)) - asin(sqrt(score1)))
weight <- dnorm(fis, mean = mu2, sd = sqrt(sigmasquared2))

# Calculate the weighted mean of h
weighted_mean_h <- sum(h * weight) / sum(weight)

# Print the weighted mean of h
print(weighted_mean_h)

2*(asin(sqrt(expected_score(0.011,a2,b2)))-asin(sqrt(expected_score(0.011,a1,b1))))

# Summary of the vectors
summary(fis)
summary(score1)
summary(score2)
summary(h)
summary(weight)

# Plot score2 over fis as a simple line plot
plot(fis, h, type = "l")

