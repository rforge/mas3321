# Fig 
theta=seq(0,5,len=1000)
plot(theta,dgamma(theta,8,9),type="l",xlab=expression(theta),ylab="density")
lines(theta,dgamma(theta,2,1),lty=2)

# Fig 
theta=seq(0,3,len=1000)
plot(theta,dgamma(theta,8,9),type="l",xlab=expression(theta),ylab="density")

hdiBeta(0.95,8,9)

# Fig 
mu=seq(5.1,5.9,len=1000)
plot(mu,dnorm(mu,5.4839,0.0415),type="l",xlab=expression(mu),ylab="density")
lines(mu,dnorm(mu,5.4,0.2),lty=2)

