# analysis of Malcolm's data
data(malcolm)

# Fig 2.7
plot(malcolm$shoe,malcolm$height,xlab="shoe",ylab="height")

m=lm(height ~ shoe, x=T, data=malcolm)
betahat=m$coefficients
XtX=t(m$x)%*%m$x
RSS=sum(residuals(m)^2)

b=c(167/3,5/3)
c=matrix(c(0.8889,7.1111,7.1111,64.8889),ncol=2)
g=2
h=4

B=solve(c+XtX,c%*%b+XtX%*%betahat)
C=c+XtX
G=g+nrow(malcolm)/2
H=as.numeric(h+0.5*(RSS-t(B)%*%(c+XtX)%*%B+t(b)%*%c%*%b+t(betahat)%*%XtX%*%betahat))
cinv=solve(c,diag(1,2))
Cinv=solve(C,diag(1,2))

# prior and posterior variance matrices
(h/(g-1))*cinv
(H/(G-1))*Cinv
# prior and posterior correlations
cinv[1,2]/sqrt(cinv[1,1]*cinv[2,2])
Cinv[1,2]/sqrt(Cinv[1,1]*Cinv[2,2])


# Fig 2.8
beta1=seq(45,65,len=1000)
plot(beta1,dgt(beta1,2*G,B[1],(H/G)*Cinv[1,1]),type="l",xlab=expression(alpha),ylab="density")
lines(beta1,dgt(beta1,2*g,b[1],(h/g)*cinv[1,1]),lty=2)

beta2=seq(0,3,len=1000)
plot(beta2,dgt(beta2,2*G,B[2],(H/G)*Cinv[2,2]),type="l",xlab=expression(beta),ylab="density")
lines(beta2,dgt(beta2,2*g,b[2],(h/g)*cinv[2,2]),lty=2)

tau=seq(0,1.5,len=1000)
plot(tau,dgamma(tau,G,H),type="l",xlab=expression(tau),ylab="density")
lines(tau,dgamma(tau,g,h),lty=2)

sigma=seq(0,3,len=1000)
plot(sigma,dinvchi(sigma,G,H),type="l",xlab=expression(sigma),ylab="density")
lines(sigma,dinvchi(sigma,g,h),type="l",lty=2)


# Fig 2.9
beta1=seq(46,65,len=1000)
beta2=seq(0.5,3,len=1000)
tcontour(beta1,beta2,2*g,b,(h/g)*cinv,xlab=expression(alpha),ylab=expression(beta),lty=3)
tcontour(beta1,beta2,2*G,B,(H/G)*Cinv,add=TRUE)

beta1=seq(54,57.5,len=1000)
beta2=seq(1.4,1.8,len=1000)
tcontour(beta1,beta2,2*g,b,(h/g)*cinv,xlab=expression(alpha),ylab=expression(beta),lty=3)
tcontour(beta1,beta2,2*G,B,(H/G)*Cinv,add=TRUE)

# 95% HDI bounds for alpha and beta
c(B[1]-qt(0.975,2*G)*sqrt(H*Cinv[1,1]/G),B[1]+qt(0.975,2*G)*sqrt(H*Cinv[1,1]/G))
c(B[2]-qt(0.975,2*G)*sqrt(H*Cinv[2,2]/G),B[2]+qt(0.975,2*G)*sqrt(H*Cinv[2,2]/G))

# 95% HDI bounds for tau and sigma
hdiGamma(0.95,G,H)

hdiInvchi(0.95,G,H)

# 95% equi-tailed CI for tau and sigma
c(qgamma(0.025,G,H),qgamma(0.975,G,H))

c(1/sqrt(qgamma(0.975,G,H)),1/sqrt(qgamma(0.025,G,H)))

# Fig 2.10
beta1=seq(39,73,len=1000)
beta2=seq(-0.5,4,len=1000)
tcontour(beta1,beta2,2*g,b,(h/g)*cinv,p=c(0.95,0.9,0.8),xlab=expression(alpha),ylab=expression(beta),lty=3)
tcontour(beta1,beta2,2*G,B,(H/G)*Cinv,p=c(0.95,0.9,0.8),add=TRUE)

beta1=seq(54,57.5,len=1000)
beta2=seq(1.39,1.8,len=1000)
tcontour(beta1,beta2,2*g,b,(h/g)*cinv,p=c(0.95,0.9,0.8),xlab=expression(alpha),ylab=expression(beta),lty=3)
tcontour(beta1,beta2,2*G,B,(H/G)*Cinv,p=c(0.95,0.9,0.8),add=TRUE)

