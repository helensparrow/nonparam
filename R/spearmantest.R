#' Calculates Spearman's correlation
#'
#' @param X A vector of numbers
#' @param Y A vector of numbers
#' @return The spearman correlation between \code{X} and \code{Y}
#' @examples
#' spearmantest(c(1,2,3), c(6,5,4))
#' spearmantest(10, 1)
#'

library(SuppDists)

spearmantest <- function(X, Y) {
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
