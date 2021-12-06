#' Wilcoxon Correlation
#'
#' @description The Wilcoxon test statistic and corresponding p-value, from one of three alternative hypotheses
#' @param X A vector of numbers
#' @param Y A vector of numbers
#' @param alpha Alpha value
#' @param alternative One of 'two.sided' (default), 'greater', or 'less'
#' @param digits The desired number of digits after the decimal point
#' @return The wilcoxon test statistic and p-value between \code{X} and \code{Y}, calculated both exactly and asymptotically
#' @examples
#' wilcoxon(c(1,2,3), c(6,5,4))
#' wilcoxon(rnorm(15, 0, 1), rnorm(15, 5, 1))
#'

wilcoxon <- function(X, Y, alpha = 0.05, alternative = "two.sided", digits = 4) {
  alpha <- ifelse(alternative == "two.sided", alpha/2, alpha)

  # Wilcoxon Two-Sample Test
  m <- length(X)
  n <- length(Y)

  # test statistics
  W.pre <- data.frame(Values = c(X, Y),
                      Data = c(rep("X",length(X)), rep("Y",length(Y))),
                      Ranks = rank(c(X, Y)))
  W = sum(W.pre$Ranks[W.pre$Data =="Y"])
  U <- sum(outer(X, Y, FUN = "<"))

  # Exact
  # critical values
  walpha <- cv.upper <- qwilcox(1-alpha, m, n) + 1
  cv.lower <- n*m - walpha
  cv.wilcox <- c(ifelse(alternative == "greater", NA, cv.lower),
                 ifelse(alternative == "less", NA, cv.upper))

  # Asymptotic
  # critical values
  eu <- m * n / 2
  varu <- m * n * (m + n + 1) / 12

  cv.wilcox.a <- qnorm(c(ifelse(alternative == "greater", NA, alpha),
                         ifelse(alternative == "less", NA, 1 - alpha)),
                       eu, sqrt(varu))


  if(alternative == "two.sided") {
    actAlpha <- pwilcox(cv.lower, m, n) + pwilcox(cv.upper-1, m, n, lower.tail=F) # P(X<= cv.l) + P(X>= cv.u)
    p.val <- ifelse(U < m*n/2, 2*pwilcox(U, m, n), 2*pwilcox(U-1, m, n, lower.tail=F))

    actAlpha.a <- alpha * 2
    p.val.a <- 2*pnorm(U, eu, sqrt(varu))
  }
  if(alternative == "greater") {
    actAlpha <- pwilcox(cv.upper-1, m, n, lower.tail = FALSE) # P(X>= cv.u)
    p.val <- pwilcox(U-1, m, n, lower.tail = FALSE)

    actAlpha.a <- alpha
    p.val.a <- pnorm(U, eu, sqrt(varu), lower.tail = FALSE)
  }
  if(alternative == "less") {
    actAlpha <- pwilcox(cv.lower, m, n) # P(X<= cv.l)
    p.val <- pwilcox(U, m, n)

    actAlpha.a <- alpha
    p.val.a <- pnorm(U, eu, sqrt(varu))
  }


  results <- data.frame('Exact' = c(W, U, cv.wilcox, p.val, actAlpha),
                        'Asymptotic' = c(NA, NA, cv.wilcox.a, p.val.a, actAlpha.a))
  results <- round(results, digits)

  results <- data.frame('Quantity' = c("Test Statistic: W", "Test Statistic: U", "CV Lower",
                                       "CV Upper", "P-Value", "Actual Alpha"),
                        results)

  return(results)
}

