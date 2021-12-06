#' Kendall Correlation
#'
#' @description This requires the SuppDists library to run; which is a package from CRAN, and can be installed and loaded with \code{install.packages("SuppDists")} and \code{library(SuppDists)}
#'
#' @param X A vector of numbers
#' @param Y A vector of numbers
#' @return The kendall correlation between \code{X} and \code{Y}
#' @examples
#' kendall(c(1,2,3), c(6,5,4))
#' kendall(rnorm(15, 0, 1), rnorm(15, 5, 1))
#'





kendall <- function(X, Y) {
  n <- length(X)

  Q <- function(pi, pj) { ifelse((pj[1]-pi[1])*(pj[2]-pi[2]) < 0, -1, 1) }
  Qstar<- function(pi, pj) { ifelse((pj[1]-pi[1])*(pj[2]-pi[2]) < 0, -1,
                                    ifelse((pj[1]-pi[1])*(pj[2]-pi[2]) > 0, 1, 0)) }

  Qij <- numeric(length = n*(n-1)/2)
  counter <- 1
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      Qij[counter] <- Qstar(X[i], Y[j])
      counter <- counter + 1
    }
  }

  K <- sum(Qij)
  Kbar <- K/(n*(n-1)/2)

  # Critical Value
  kalpha <- qKendall(p = 0.95, N = n)

  # P-Value
  p.val <- 1 - pKendall(Kbar, N = n)

  results <- data.frame('Quantity' = c("Test Statistic (K)", "Test Statistic (KBar)","Critical Value",
                                       "P-Value"),
                        'By-Hand' = c(K, Kbar, kalpha, p.val),
                        'Built-In Func' = c(NA, tt$estimate, NA, tt$p.value))
  return(results)
}
