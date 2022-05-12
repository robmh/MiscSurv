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
#' ## Simulating one random sample data from the survival function.
#' time <- 0:100
#' prob <- exp(-time*.05)
#' n <- 10
#' x <- simu_deaths(time,prob,n)
#' 
#' ## Inverse approach: approximating the survival function from simulations.
#' ## We build a more complicated survival function, one which is not sampled
#' ## regularly and has flat parts.
#' time <- c(1,3,4,8,9,12:17)
#' prob <- c(1,1,1,.9,.8,.8,.7,.5,.2,.2,.2)
#' n <- 10
#' nrep <- 100
#' y <- NULL
#' for (i in 1:nrep) {
#' z <- simu_deaths(time,prob,n)
#' y <- c(y,z$Death[z$Event==1])
#' }
#' p <- sapply(time,function(i) sum(y==i))
#' p <- cumsum(p)
#' r <- lm(prob~p)
#' par(mfcol=c(2,1),mar=c(3,4,3,2))
#' plot(time,predict(r),xlab="Time",ylab="Survival",ylim=c(0,1))
#' points(time,prob,type="l",lwd=2)
#' plot(time,prob-predict(r),xlab="Time",ylab="Differences")
#' par(mfcol=c(1,1))
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
  
  if (sum(is.na(df))>0) browser()

  return(df)
}
