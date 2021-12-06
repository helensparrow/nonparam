#' Spearman Correlation
#'
#' @description This requires the SuppDists library to run; which is a package from CRAN, and can be installed and loaded with \code{install.packages("SuppDists")} and \code{library(SuppDists)}
#'
#' @param X A vector of numbers
#' @param Y A vector of numbers
#' @return The spearman correlation between \code{X} and \code{Y}
#' @examples
#' spearman(c(1,2,3), c(6,5,4))
#' spearman(rnorm(15, 0, 1), rnorm(15, 5, 1))
#'


spearman <- function(X, Y) {
  n <- length(X)

  Si <- rank(X)
  Ri <- rank(Y)

  rs <- 1 - (6*sum((Si-Ri)^2))/(n*(n^2-1))

  rsalpha <- qSpearman(p = 0.95, r = n)

  # P-Value
  pval <- (1-pSpearman(rs, n))

  results <- data.frame('Quantity' = c("Test Statistic","Critical Value", "P-Value"),
                        'Value' = c(rs, rsalpha, pval))
  return(results)
}
