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
#'
#'
ntickets <- function(N, gamma, p) {
  qbinom <- qnorm <- pbinom <- pnorm <- NULL
  index_of_smallest_d <- index_of_smallest <- abline <- NULL

  #1:create sequence of numbers from N to N+N/10 (they are proportional)
  #2:get inverse cdf, subtract N. This will center n at 0 and make it easy
  #to find minimum.
  #3:the index of n is found using which.min. abs(binom_n) ensures that the minimum
  #found is 0.
  #4:the index is plugged into the sequence, getting n and storing it in nd
  seqb <- seq(N, round(N+N/10), by=1)
  binom_n <- qbinom(1-gamma,seqb,p)-N
  mIndexb <- which.min(abs(binom_n))
  nd <- seqb[mIndexb]

  #Same process as above, but with the normal function instead. Each segment is small
  #to create a continuous-looking function.
  seqn <- seq(N, round(N+N/10), by=0.001)
  normal_n <- qnorm(1-gamma,mean=(seqn*p),sd=(sqrt((seqn*p)*(1-p))))-N
  mIndexn <- which.min(abs(normal_n))
  nc <- seqn[mIndexn]


  #The objective function of the discrete distribution is graphed.
  n_disc <- seq(N, round(N+N/10), by=1)
  objective_disc <- 1-gamma-pbinom(N, size=n_disc, p=p)
  thepasted= paste("Objective Vs n to find optimal tickets sold\n(", nd,
                   ") gamma=", gamma,"N=",N,"discrete")
  plot(n_disc, objective_disc,xlab="n",ylab="Objective",main=thepasted,cex=0.8)
  abline(h=objective_disc[mIndexb], v=nd, col="red")

  #The objective function of the continuous function is graphed.
  n_cont <- seq(N, round(N+N/10), by =0.001)
  objective_cont <- 1-gamma-pnorm(N, mean=p * n_cont, sd = sqrt(n_cont * p * (1-p)))
  #index_cont <- which.min(abs(objective_cont))
  thepastec = paste("Objective Vs n to find optimal tickets sold\n(", nc,
                   ") gamma=", gamma,"N=",N,"continuous")

  plot(n_cont, objective_cont,xlab="n",ylab="Objective",main=thepastec,cex=0.4)
  abline(h=objective_cont[mIndexn], v=nc, col="blue")

  thelist <- list(
    nd=nd,
    nc=nc,
    N=N,
    p=p,
    gamma=gamma
  )
  print(thelist)

}


