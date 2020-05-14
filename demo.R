# Companion Code for Bayesian Statistics: Counting for Nerds
library(glue)

# 1,000 points evently spaced between (0, 1)
theta <- ppoints(1000)

# Noninformative Prior Distribution
flat_prior <- dbeta(theta, 1, 1)
alt_prior <- dbeta(theta, 0.5, 0.5)
non_flat_prior <- dbeta(theta, 2, 2)

plot(theta, flat_prior, type="l", col="black", ylab="Density", xlab=expression(theta), ylim=c(0, 3), main="Beta Distribution")
lines(theta, alt_prior, type="l", col="blue", ylab="Density", xlab=expression(theta), ylim=c(0,3))
lines(theta, non_flat_prior, type="l", col="green", ylab="Density", xlab=expression(theta), ylim=c(0,3))
legend(0.25,3, c("Beta(1,1)","Beta(1/2,1/2)","Beta(2, 2)"),col=c("black","blue","green"), lwd=2)

# Simulate first 100 observiations
n <- 100
incidence_rate <- 0.1
n_success <- 1
n_fail <- 1
for (i in 1:100) {
  obs <- rbinom(1, 1, incidence_rate)
  if (obs == 0) {
    n_fail <- n_fail + 1
  }
  else {
    n_success <- n_success + 1
  }
  png(glue("./images/gif/posterior_{i}.png"))
  plot(theta, dbeta(theta, n_success, n_fail), main=paste("Test # ", i), type="l", lwd=2, ylab="Density", xlab="Posterior IR")
  dev.off()
}

# Noninformative prior, day 1 posterior
n_successes <- 1
n_failures <- 1

n_days <- 5
n_successes_by_day <- c(2, 2, 1, 3, 3)
n_failures_by_day <- c(198, 148, 299, 247, 198)
theta <- ppoints(10000)*0.05

for (i in 1:n_days) {
  n_successes <- n_successes + n_successes_by_day[i]
  n_failures <- n_failures + n_failures_by_day[i]
  
  low <- qbeta(0.025, n_successes, n_failures)
  high <- qbeta(0.975, n_successes, n_failures)
  
  png(glue("./images/covid_gif/posterior_day_{i}.png"))  
  plot(theta, dbeta(theta, n_successes, n_failures), main=paste("Day", i), type="l", lwd=2, ylab="Density", xlab="Posterior IR", xlim=c(-0.01, 0.05), ylim=c(0, 200))
  abline(v=low, col="blue", lwd=2, lty=2)
  text(x=low - 0.0075, y=100, round(low, 3))
  abline(v=high, col="blue", lwd=2, lty=2)
  text(x=high + 0.0075, y=100, round(high,  3)) 
  dev.off()
}
