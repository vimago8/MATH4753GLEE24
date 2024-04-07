#' Normal Distribution
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a p < a
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{myncurve(10, 5, 2)}
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma),ylim=c(0, 0.3989/sigma),col="Red",ylab="Normal Density")
  x=seq(0, a, length=1000)
  y=dnorm(x, mean=mu,sd=sigma )
  polygon(c(0, x, a), c(0, y, 0), col="Red")

  prob = pnorm(a, mean=mu, sd=sigma)

  var <- list(mu = mu, sigma = sigma, area = round(prob,4))
}
