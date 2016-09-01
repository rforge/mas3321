# Cavendish data analysis
data(cavendish)
b=5.41;c=0.25;g=2.5;h=0.1
xbar=mean(cavendish$earth_density)
n=length(cavendish$earth_density)
s=sqrt((n-1)*var(cavendish$earth_density)/n)
B=(b*c+n*xbar)/(c+n)
C=c+n
G=g+n/2
H=h+c*n*(xbar-b)^2/(2*(c+n))+n*s^2/2

# Fig 2.3
mu=seq(4,7,len=1000)
plot(mu,dgt(mu,2*g,b,h/(g*c)),type="l",xlab=expression(mu),ylab="density",ylim=c(0,1))
lines(mu,dnorm(mu,5.41,0.4),lty=2)

tau=seq(0,70,len=1000)
plot(tau,dgamma(tau,g,h),type="l",xlab=expression(tau),ylab="density")

sigma=seq(0,0.5,len=1000)
plot(sigma,dinvchi(sigma,g,h),type="l",xlab=expression(sigma),ylab="density")

# Fig 2.4
mu=seq(4,7,len=1000)
plot(mu,dgt(mu,2*G,B,H/(G*C)),type="l",xlab=expression(mu),ylab="density")
lines(mu,dgt(mu,2*g,b,h/(g*c)),lty=2)

tau=seq(0,70,len=1000)
plot(tau,dgamma(tau,G,H),type="l",xlab=expression(tau),ylab="density")
lines(tau,dgamma(tau,g,h),lty=2)

sigma=seq(0,0.5,len=1000)
plot(sigma,dinvchi(sigma,G,H),type="l",xlab=expression(sigma),ylab="density")
lines(sigma,dinvchi(sigma,g,h),type="l",lty=2)

# Fig 2.5
mu=seq(4.5,6.5,len=1000)
tau=seq(0,71,len=1000)
NGacontour(mu,tau,b,c,g,h,lty=3)
NGacontour(mu,tau,B,C,G,H,add=TRUE)

# Fig 2.6
mu=seq(3.5,7.5,len=1000)
tau=seq(0,80,len=1000)
NGacontour(mu,tau,b,c,g,h,p=c(0.95,0.9,0.8),lty=3)
NGacontour(mu,tau,B,C,G,H,p=c(0.95,0.9,0.8),add=TRUE)

mu=seq(5.3,5.7,len=1000)
tau=seq(10,60,len=1000)
NGacontour(mu,tau,b,c,g,h,p=c(0.95,0.9,0.8),lty=3)
NGacontour(mu,tau,B,C,G,H,p=c(0.95,0.9,0.8),add=TRUE)
