# Cavendish data analysis
data(cavendish)
m=lm(earth_density~1,x=T,data=cavendish)
betahat=m$coefficients
XtX=t(m$x)%*%m$x
RSS=sum(residuals(m)^2)
b=5.41;c=matrix(0.25,ncol=1);g=2.5;h=0.1
B=solve(c+XtX,c%*%b+XtX%*%betahat)
C=c+XtX
G=g+nrow(cavendish)/2
H=h+0.5*(RSS-t(B)%*%(c+XtX)%*%B+t(b)%*%c%*%b+t(betahat)%*%XtX%*%betahat)

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
lines(mu,dt(mu,2*g,b,h/(g*c)),lty=2)

tau=seq(0,70,len=1000)
plot(tau,dgamma(tau,G,H),type="l",xlab=expression(tau),ylab="density")
lines(tau,dgamma(tau,g,h),lty=2)

sigma=seq(0,0.5,len=1000)
plot(sigma,dinvchi(sigma,G,H),type="l",xlab=expression(sigma),ylab="density")
lines(sigma,dinvchi(sigma,g,h),type="l",lty=2)

# Fig 2.5
mu=seq(4.5,6.5,len=100)
tau=seq(0,70,len=100)
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
