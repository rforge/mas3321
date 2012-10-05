# source("R.txt")

# Fig 
xgrid=seq(-3,3,len=100)
ygrid=seq(-3,3,len=100)
rho=0.5
z=outer(xgrid,ygrid,dbvnorm)

#mypdf("bivnorm")
persp(xgrid,ygrid,z,theta=10,phi=10,xlab="",ylab="",zlab="",ticktype="detailed")
#dev.off()

#pdf("normalcontours.pdf",height=4.5,width=4.5)
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
#layout.show(nf)
par(mar=c(3,3,1,1))
contour(xgrid,ygrid,z)
lines(c(-2,2),c(-2,2))
lines(c(-1.3,1.3),c(1.3,-1.3))
par(mar=c(0,3,1,1))
plot(xgrid,dnorm(xgrid),type="l")
par(mar=c(3,0,1,1))
plot(dnorm(ygrid),ygrid,type="l")
#dev.off()


# Cavendish data analysis
y=c(5.36,5.29,5.58,5.65,5.57,5.53,5.62,5.29,5.44,5.34,5.79,5.10,5.27,5.39,5.42,5.47,5.63,5.34,5.46,5.30,5.78,5.68,5.85)
cav=data.frame(y=y)
m=lm(y~1,x=T,data=cav)
betahat=m$coefficients
XtX=t(m$x)%*%m$x
RSS=sum(residuals(m)^2)
b=5.41;c=matrix(0.25,ncol=1);g=2.5;h=0.1
B=solve(c+XtX,c%*%b+XtX%*%betahat)
C=c+XtX
G=g+length(y)/2
H=h+0.5*(RSS-t(B)%*%(c+XtX)%*%B+t(b)%*%c%*%b+t(betahat)%*%XtX%*%betahat)

# Fig 
#myspdf("cav-prior-mu")
mu=seq(4,7,len=1000)
plot(mu,dt((mu-5.41)/sqrt(0.16),5)/sqrt(0.16),type="l",xlab=expression(mu),ylab="density",ylim=c(0,1))
lines(mu,dnorm(mu,5.41,0.4),lty=2)
#dev.off()

# Fig 
#myspdf("cav-prior-tau")
tau=seq(0,70,len=1000)
plot(tau,dgamma(tau,2.5,0.1),type="l",xlab=expression(tau),ylab="density")
dev.off()

# Fig  - don't put this in the notes!!
#myspdf("cav-prior-sigma")
sigma=seq(0,0.5,len=1000)
plot(sigma,dinvchi(sigma,2.5,0.1),type="l",xlab=expression(sigma),ylab="density")
#dev.off()

# Fig 
#myspdf("cav-post-mu")
mu=seq(4,7,len=1000)
plot(mu,dt((mu-5.484)/sqrt(0.001561),28)/sqrt(0.001561),type="l",xlab=expression(mu),ylab="density")
lines(mu,dt((mu-5.41)/sqrt(0.16),5)/sqrt(0.16),lty=2)
#dev.off()

# Fig 
#myspdf("cav-post-tau")
tau=seq(0,70,len=1000)
plot(tau,dgamma(tau,14,0.5264),type="l",xlab=expression(tau),ylab="density")
lines(tau,dgamma(tau,2.5,0.1),lty=2)
#dev.off()

# Fig  - don't put this in the notes!!
#myspdf("cav-post-sigma")
sigma=seq(0,0.5,len=1000)
plot(sigma,dinvchi(sigma,14,0.5264),type="l",xlab=expression(sigma),ylab="density")
lines(sigma,dinvchi(sigma,2.5,0.1),type="l",lty=2)
#dev.off()

# Fig 
#mys4pdf("cav-post-joint")
mugrid=seq(4.5,6.5,len=100)
taugrid=seq(0,70,len=100)
prior=function(mu,tau){dnormgamma(mu,tau,5.41,0.25,2.5,0.1)}
posterior=function(mu,tau){dnormgamma(mu,tau,5.4840,23.25,14,0.5080)}
z1=outer(mugrid,taugrid,prior)
z2=outer(mugrid,taugrid,posterior)
contour(mugrid,taugrid,z1,xlab=expression(mu),ylab=expression(tau),lty=3)
contour(mugrid,taugrid,z2,add=T)
#dev.off()

