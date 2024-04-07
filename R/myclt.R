#' myclt
#'
#' @param n n
#' @param iter iter
#'
#' @return
#' @export
#'
#' @description
#' The sm has been changed to head(sm) to avoid excessive output
#'
#'
#' @examples
#' \dontrun{myclt(n=10,iter=10000)}
myclt=function(n,iter){
  y=runif(n*iter,0,5)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  hist(sm)
  head(sm)
}
