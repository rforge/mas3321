# analysis of blood coagulation times
data(coagulation)

m=lm(coag~factor(diet)-1,x=T,data=coagulation)
betahat=m$coefficients
XtX=t(m$x)%*%m$x
RSS=sum(residuals(m)^2)

b=c(60,60,60)
c=4.94*matrix(c(1,-0.4,-0.4,-0.4,1,-0.4,-0.4,-0.4,1),ncol=3)
g=24/9
h=24

B=solve(c+XtX,c%*%b+XtX%*%betahat)
C=c+XtX
G=g+nrow(coagulation)/2
H=as.numeric(h+0.5*(RSS-t(B)%*%(c+XtX)%*%B+t(b)%*%c%*%b+t(betahat)%*%XtX%*%betahat))
cinv=solve(c,diag(1,3))
Cinv=solve(C,diag(1,3))


# Fig 4.5
mu1=seq(54,68,len=1000)
plot(mu1,dgt(mu1,2*G,B[1],(H/G)*Cinv[1,1]),type="l",xlab=expression(mu[1]),ylab="density")
lines(mu1,dgt(mu1,2*g,b[1],(h/g)*cinv[1,1]),lty=2)

mu2=seq(54,68,len=1000)
plot(mu2,dgt(mu2,2*G,B[2],(H/G)*Cinv[2,2]),type="l",xlab=expression(mu[2]),ylab="density")
lines(mu2,dgt(mu2,2*g,b[2],(h/g)*cinv[2,2]),lty=2)

mu3=seq(54,68,len=1000)
plot(mu3,dgt(mu3,2*G,B[3],(H/G)*Cinv[3,3]),type="l",xlab=expression(mu[3]),ylab="density")
lines(mu3,dgt(mu3,2*g,b[3],(h/g)*cinv[3,3]),lty=2)

tau=seq(0,0.4,len=1000)
plot(tau,dgamma(tau,G,H),type="l",xlab=expression(tau),ylab="density")
lines(tau,dgamma(tau,g,h),lty=2)

sigma=seq(0,10,len=1000)
plot(sigma,dinvchi(sigma,G,H),type="l",xlab=expression(sigma),ylab="density")
lines(sigma,dinvchi(sigma,g,h),type="l",lty=2)

# 95% HDI bounds for mu_1, mu_2, mu_3
B[1]-qt(0.975,2*G)*sqrt(H*Cinv[1,1]/G)
B[1]+qt(0.975,2*G)*sqrt(H*Cinv[1,1]/G)
B[2]-qt(0.975,2*G)*sqrt(H*Cinv[2,2]/G)
B[2]+qt(0.975,2*G)*sqrt(H*Cinv[2,2]/G)
B[3]-qt(0.975,2*G)*sqrt(H*Cinv[3,3]/G)
B[3]+qt(0.975,2*G)*sqrt(H*Cinv[3,3]/G)

# 95% HDI bounds for tau and sigma
hdiGamma(0.95,G,H)

hdiInvchi(0.95,G,H)

# 95% equi-tailed CI for tau and sigma
qgamma(0.025,G,H);qgamma(0.975,G,H)

1/sqrt(qgamma(0.975,G,H));1/sqrt(qgamma(0.025,G,H))

A=matrix(c(-1,-1,1,0,0,1),ncol=3)
bs=A%*%b
cs=solve(A%*%c%*%t(A),diag(1,2))
csinv=solve(cs,diag(1,2))
Bs=A%*%B
Cs=solve(A%*%Cinv%*%t(A),diag(1,2))
Csinv=solve(Cs,diag(1,2))

# prior mean, variance and correlation
bs
(h/(g-1))*csinv
csinv[1,2]/sqrt(csinv[1,1]*csinv[2,2])
# posterior mean, variance and correlation
Bs
(H/(G-1))*Csinv
Csinv[1,2]/sqrt(Csinv[1,1]*Csinv[2,2])

# Fig 4.6
alpha2=seq(-20,20,len=1000)
plot(alpha2,dgt(alpha2,2*G,Bs[1],(H/G)*Csinv[1,1]),type="l",xlab=expression(alpha[2]),ylab="density")
lines(alpha2,dgt(alpha2,2*g,bs[1],(h/g)*csinv[1,1]),lty=2)

alpha3=seq(-20,20,len=1000)
plot(alpha3,dgt(alpha3,2*G,Bs[2],(H/G)*Csinv[2,2]),type="l",xlab=expression(alpha[3]),ylab="density")
lines(alpha3,dgt(alpha3,2*g,bs[2],(h/g)*csinv[2,2]),lty=2)

# Fig 4.7
alpha2=seq(-10,10,len=1000)
alpha3=seq(-10,10,len=1000)
tcontour(alpha2,alpha3,2*g,bs,(h/g)*csinv,xlab=expression(alpha[2]),ylab=expression(alpha[3]),lty=3)
tcontour(alpha2,alpha3,2*G,Bs,(H/G)*Csinv,add=TRUE)

# 95% HDI bounds for alpha_1 and alpha_2
c(qgt(0.025,2*G,Bs[1],Csinv[1,1]*H/G),qgt(0.975,2*G,Bs[1],Csinv[1,1]*H/G))
c(qgt(0.025,2*G,Bs[2],Csinv[2,2]*H/G),qgt(0.975,2*G,Bs[2],Csinv[2,2]*H/G))

# Fig 4.8
alpha2=seq(-40,40,len=1000)
alpha3=seq(-40,40,len=1000)
tcontour(alpha2,alpha3,2*g,bs,(h/g)*csinv,p=c(0.95,0.9,0.8),xlab=expression(alpha[2]),ylab=expression(alpha[3]),lty=3)
tcontour(alpha2,alpha3,2*G,Bs,(H/G)*Csinv,p=c(0.95,0.9,0.8),lwd=2,add=TRUE)

alpha2=seq(-2,6,len=1000)
alpha3=seq(-4,4,len=1000)
tcontour(alpha2,alpha3,2*g,bs,(h/g)*csinv,p=c(0.95,0.9,0.8),xlab=expression(alpha[2]),ylab=expression(alpha[3]),lty=3)
tcontour(alpha2,alpha3,2*G,Bs,(H/G)*Csinv,p=c(0.95,0.9,0.8),lwd=2,add=TRUE)
