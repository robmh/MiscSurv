#' Simulation of survival data set
#'
#' It simulates a data set of right-censored n individuals for a
#' given survival function.
#'
#' @param time time points at which survival is evaluated.
#' @param prob survival probability density function.
#' @param n number of individuals to simulate.
#'
#' @details
#' To be completed.
#'
#' @return \code{data.frame} with two columns named \code{Death} and \code{Event}.
#' The former yields the \code{time} of the event and the latter is the status
#' indicates, i.e. dead (\code{Event=1}) or alive (i.e. \code{Event=0}).
#' 
#' @examples
#'
#' ##
#' time <- 0:100
#' prob <- exp(-time*.05)
#' n <- 120
#' x <- simu_deaths(time,prob,n)
#' 
#' ## Simulating the original survival function.
#' nrep <- 100
#' y <- replicate(nrep,simu_deaths(time,prob,n)$Death)
#' p <- sapply(0:99,function(i) sum(y==i))
#' p <- p/max(p)
#' plot(time[-100],p,xlab="Time",ylab="Survival")
#' points(time,prob,type="l",lwd=2)
#'
#' @export
# 
simu_deaths <- function(time,prob,n) {
  
  # Random probabilities
  pr <- runif(n, min=0, max=1)
  
  # Calculate in which interval they fall.
  x <- outer(prob,pr,">=")
  
  # Find the position of the last -1.
  j <- apply(x,2,function(q) max(which(q)))
  
  # The next position is what we are interested in.
  j <- j+1

  # Count how many alive individuals at the end of "time" there are.
  length_time <- length(time)
  n0 <- sum(j>length_time)

  # If there are no 0's, it's easy.
  if (n0==0) {
    df <- data.frame(Death=time[j],Event=1)
    
  # If there are no -1's (i.e. no deaths), it's even easier.
  } else if (n0==n) {
    df <- data.frame(Death=numeric(n),Event=0)
    
  # It's more complicated when there are 0's and -1's.
  } else {
    j <- j[j<=length_time]
    df <- data.frame(Death=c(rep(max(time),n0),time[j]),
                     Event=c(rep(FALSE,n0),rep(TRUE,length(j))))
  }

  return(df)
}
