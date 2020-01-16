library(MASS)

# function to create 2 correlated variables with fixed cor and SDs
ref <- function(n, xcor = 0.5, sd1 = 1, sd2 = 1, ...) {
  mat <- matrix(c(1, xcor, xcor, 1), nrow = 2)
  res <- mvrnorm(n = n, mu = rep(0, 2), Sigma = mat, ...)
  res[, 1] <- res[, 1] * sd1
  res[, 2] <- res[, 2] * sd2
  res
}

x <- ref(10)
apply(x, 2, mean)

x <- ref(10, empirical = TRUE)
apply(x, 2, mean)

