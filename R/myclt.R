#' Central Limit Theorem (CLT) function
#'
#' @param n number of trials
#' @param iter number of iterations
#'
#' @return a plot containing a histogram of the normalized distribution
#' @export
#'
#' @description
#' Note: sm has been changed to head(sm) to avoid excessive output.
#'
#' This function demonstrates CLT using a uniform distribution. CLT states that
#' the distribution of a sample variable approximates a normal distribution,
#' regardless of the actual distribution shape.
#'
#'
#'
#'
#'
#'
myclt=function(n,iter){
  runif <- NULL
  hist <- NULL
  head <- NULL
  y=runif(n*iter,0,5)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  hist(sm)
  head(sm)
}
