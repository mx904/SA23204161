#' @title The decision boundary for SNSMS under significance level alpha
#' @name db.c
#' @description \code{db.c} is a (2,4,3)-dimension array. It gives the decision boundary when the significance level alpha, the monitoring scheme Tm and the dimension of samples are given 
#' @examples
#' \dontrun{
#'   data(db.c)
#'   alpha <- 1 # 1 for alpha = 0.05, 2 for alpha = 0.1
#'   Tm <- 3 # 1 for Tm = 1, 2 for Tm = 2, 3 for Tm = 10 and 4 for Tm > 100
#'   d <- 1 # d = 1, 2 or 3
#'   if(Mm > db.c[alpha,Tm,d]){ # Mm is the test statistic, a function of time k
#'     print(k)
#'   }
#' }
NULL