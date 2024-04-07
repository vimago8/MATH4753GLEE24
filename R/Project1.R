#' Airline Overbooking Function
#' @description
#' Calculates the optimal number of seats to sell given the number of seats,
#'probability of overbooking, and the probability that a passenger will show up.
#'The problem can be modeled using a discrete (binomial) distribution or a
#'continuous (normal) distribution.
#'
#'
#' @param N The number of seats in the flight
#' @param gamma The probability of overbooking
#' @param p The probabiility that a passenger will show up
#'
#' @return A list containing nd, nc, n, p, and gamma.
#'
#' @details
#' To calculate n for the discrete distribution (binomial), the function creates
#' a sequence of values from N to round(N+N/10), separated by 1. These values are
#' used to create a vector of (1-gamma)th quantiles using different "trials" (number
#' of tickets). This vector is subtracted by N so that the index of the minimum value, which
#' is 0, may be found. This index gives n when plugged into the sequence.
#'
#' The same applies for the continuous distribution (normal), except it is the inverse cdf of the
#' normal distribution. The sequences are separated by small, fractional values instead.
#'
#' The objective functions of the discrete and continuous are calculated the same way, except with
#' 1-gamma-pbinom(...) and 1-gamma-pnorm(...)
#'
#'
#'
#'
#'
#'
#' @export
#'
#' @examples
#' \dontrun{ntickets(N=200,gamma=0.02,p=0.95)}
ntickets <- function(N, gamma, p) {
#Creates a sequence of values from N to round(N+N/10), separated by 1. These values
#are used to create a vector of (1-gamma)th quantiles using different "trials" (number
#) of tickets.

  seqb <- seq(N, round(N+N/10), by=1)
  binom_n <- qbinom(1-gamma,seqb,p)-N
  mIndexb <- which.min(abs(binom_n))
  nd <- seqb[mIndexb]

  seqn <- seq(N, round(N+N/10), by=0.00001)
  normal_n <- qnorm(1-gamma,mean=(seqn*p),sd=(sqrt((seqn*p)*(1-p))))-N
  mIndexn <- which.min(abs(normal_n))
  nc <- seqn[mIndexn]

  n_disc <- seq(N, round(N+N/10), by=1)
  objective_disc <- 1-gamma-pbinom(N, size=n_disc, p=p)
  thepasted= paste("Objective Vs n to find optimal tickets sold(", nd,
                   ") \n gamma=", gamma,"N=",N,"discrete")
  plot(n_disc, objective_disc,xlab="n",ylab="Objective",main=thepasted,cex=0.8)
  abline(h=objective_disc[index_of_smallest_d], v=nd, col="red")

  #############
  n_cont <- seq(N, round(N+N/10), by =0.01)
  objective_cont <- 1-gamma-pnorm(N, mean=p * n_cont, sd = sqrt(n_cont * p * (1-p)))
  thepastec = paste("Objective Vs n to find optimal tickets sold(", nc,
                   ") \n gamma=", gamma,"N=",N,"continuous")

  plot(n_cont, objective_cont,xlab="n",ylab="Objective",main=thepastec,cex=0.4)
  abline(h=objective_cont[index_of_smallest], v=nc, col="blue")

  thelist <- list(
    nd=nd,
    nc=nc,
    N=N,
    p=p,
    gamma=gamma
  )
  print(thelist)

}


