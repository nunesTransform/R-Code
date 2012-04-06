#################################################################################
#
# author: Simon Müller
#
#
#################################################################################

#################################################################################
#
# Load simulated and real world data:
# simulated: Burbaetal, RachdiVieu, Behenni
# real world: Fat (n = 215), Moisture (n = 100), Octane (n = 60)
# you need the library fds
# parameters ... see the function
#
#################################################################################
load.fds <- function(dataset = "RachVieu", n, ...) {
  if (data == "RachdiVieu") {
    return(ModelRachdiVieu(n))
  } else if(dataset == "Behenni") {
    return(model3(n, ...))
  } else if (dataset == "Burbaetal") {
    return(ModelBurba(n, ...))
  } else {
    library(fds)
    spectrum <- get(paste(dataset, "spectrum", sep = ""))
    values <- get(paste(dataset, "values", sep = ""))
    return(list(X = t(spectrum$y), Y = values))
  }
}

#################################################################################
# Simulated data from Rachdi and Vieu:
# Nonparametric regression for functional data: 
# automatic smoothing parameter selection
# Journal of Statistical Planning and Inference, 
# Vol. 137, No. 9. (2007), pp. 2784-2801.
#
# Args:
#   n    : Number of curves that are simulated 
#
# Output:
#   list with: Curves X, Response Y, Regressionfunction at X, discrete time t
#################################################################################
ModelRachdiVieu <- function(n) { 
  
  # The regression function
  m <- rnorm(n, 0, 5)^2 / 3
  
  # Calculate the response
  Y <- m + rnorm(n)
  
  # Calculate the curves
  X <- c()
  t <- seq(0, 1, length = 100)
  b <- rnorm(n)
  for (i in 1:n) {
    X <- rbind(X, a[i] * (t - 0.5)^2 + b[i])
  }
  
  return(list(X = X, Y = Y, m = m, t = t))
}
  
#################################################################################
# Simulated data from Behenni et al.:
# Local smoothing regres- sion with functional data
# Computational Statistics 22, 3 (2007), 353–369.
#
# Args:
#   n       : Number of curves that are simulated 
#   snr.set1: Signal to Noise ratio of set 1
#   snr.set2: Signal to Noise ratio of set 2
#################################################################################
ModelBehenni <- function(n = 100, snr.set1, snr.set2) {
  
  n.set1 <- floor(n / 2)
  n.set2 <- n - n.set1

  # The regression function
  m <- c()
  a.set1 <- rnorm(n.set1, -3, snr.set1)
  m[1:n.set1] <- 6 * a.set1
  a.set2 <- rnorm(n.set2, 4, snr.set2)
  m[(n.set2+1):n] <- 6 * a.set2

  # Calculate the response 
  Y <- m + rnorm(n)

  # Calculate the curves
  b <- rnorm(n, 0 , 3)
  t <- seq(0, 1, length = 100)
  X <- c()
  for (i in 1:n.set1) {
    X[i, ] <- 3 * a.set1[i] * (t - 0.5)^2 + b[i]
  }
  for (i in 1:n.set2) {
    X[n.set2 + i, ] <- 3 * a.set2[i] * (t - 0.5)^2 + b[n.set1 + i]
  }
  
  return(list(X = X, Y = Y, m = m, t = t))
}

#################################################################################
# Simulated data from Burba et al.:
# k-Nearest Neighbour method in functional nonparametric regression
# Journal of Nonparametric Statistics, Vol. 21, No. 4. (2009), pp. 453-469
#
# Args:
#   n    : Number of curves that are simulated 
#   sigma: The parameter that controls the homogeneity/heterogeneity of the data
#################################################################################
ModelBurba <- function(n = 100, sigma = 1) {
  n.set1 <- floor(n * 0.5)
  n.set2 <- n - n.set1

  # The regression function
  a.set1 <- rnorm(n.set1, 0, 1)
  a.set2 <- rnorm(n.set2, 3, sigma)
  m <- c(a.set1^2, a.set2^2)
  
  # Calculate the response
  Y <- m + rnorm(n, 0, 0.05)
  
  # Calculate the curves
  X <- c()
  t <- seq(0, pi, length = 100)
  for (i in 1:n.set1) {
    X <- rbind(X, a.set1[i] * cos(2 * t))
  }
  for (i in 1:n.set2) {
    X <- rbind(X, a.set2[i] * cos(2 * t))
  }
    
  return(list(X = X, Y = Y, m = m, t = t))
}