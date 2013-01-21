# Cavendish data analysis
data(cavendish)
m=lm(cav_den ~1,x=T)
betahat=m$coefficients
XtX=t(m$x)%*%m$x
RSS=sum(residuals(m)^2)
b=5.41;c=0.25;g=2.5;h=0.1
B=solve(c+XtX,c%*%b+XtX%*%betahat)
C=c+XtX
G=g+length(cav_den)/2
H=h+0.5*(RSS-t(B)%*%(c+XtX)%*%B+t(b)%*%c%*%b+t(betahat)%*%XtX%*%betahat)

# Fig 
mu=seq(3.9,7,len=100)
tau=seq(0,72,len=100)
prior=function(mu,tau){dnormgamma(mu,tau,b,c,g,h)}
posterior=function(mu,tau){dnormgamma(mu,tau,B,C,G,H)}
def.par <- par(no.readonly = TRUE) # save default, for resetting...
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
par(mar=c(3,3,1,1))
z1=outer(mu,tau,prior)
z2=outer(mu,tau,posterior)
contour(mu,tau,z1,xlab=expression(mu),ylab=expression(tau),lty=3)
contour(mu,tau,z2,add=TRUE)
par(mar=c(0,3,1,1))
plot(mu,dgt(mu,2*G,B,H/(G*C)),type="l")
lines(mu,dgt(mu,2*g,b,h*(g*c)),lty=2)
par(mar=c(3,0,1,1))
plot(dgamma(tau,G,H),tau,type="l")
lines(dgamma(tau,g,h),tau,lty=2)
par(def.par)

mu=seq(4,7,len=100)
sigma=seq(0,0.5,len=100)
prior=function(mu,sigma){dnorminvchi(mu,sigma,b,c,g,h)}
posterior=function(mu,sigma){dnorminvchi(mu,sigma,B,C,G,H)}
def.par <- par(no.readonly = TRUE) # save default, for resetting...
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
par(mar=c(3,3,1,1))
z1=outer(mu,sigma,prior)
z2=outer(mu,sigma,posterior)
contour(mu,sigma,z1,xlab=expression(mu),ylab=expression(sigma),lty=3)
contour(mu,sigma,z2,add=TRUE)
par(mar=c(0,3,1,1))
plot(mu,dgt(mu,2*G,B,H/(G*C)),type="l")
lines(mu,dgt(mu,2*g,b,h*(g*c)),lty=2)
par(mar=c(3,0,1,1))
plot(dinvchi(sigma,G,H),sigma,type="l")
lines(dinvchi(sigma,g,h),sigma,lty=2)
par(def.par)

# 95% HDIs for mu
# Prior
qgt(c(0.025,0.975),2*g,b,h/(g*c))
# Posterior
qgt(c(0.025,0.975),2*G,B,H/(G*C))

# 95% HDIs for tau
# Prior
hdiGamma(0.95,g,h)
# Posterior
hdiGamma(0.95,G,H)

# 95% HDIs for sigma
# Prior
hdiInvchi(0.95,g,h)
# Posterior
hdiInvchi(0.95,G,H)

# 80%, 90% and 95% contour ellipses for (mu,tau)
mu=seq(3.5,7.5,len=1000)
tau=seq(0,80,len=1000)
NGacontour(c(0.95,0.9,0.8),mu,tau,b,c,g,h,B,C,G,H)

readline(prompt = "Ready for simple linear regression example?")

data(malcolm)
m=lm(height ~ shoe, x=T, data=malcolm)
plot(malcolm)

abline(m)

betahat=m$coefficients
XtX=t(m$x)%*%m$x
RSS=sum(residuals(m)^2)

b=c(167/3,5/3)
c=4*solve(matrix(c(36.5,-4,-4,0.5),ncol=2),diag(1,2))
g=2
h=4

B=solve(c+XtX,c%*%b+XtX%*%betahat)
C=c+XtX
G=g + nrow(malcolm)/2
H=as.numeric(h+0.5*(RSS-t(B)%*%(c+XtX)%*%B+t(b)%*%c%*%b+t(betahat)%*%XtX%*%betahat))
cinv=solve(c,diag(1,2))
Cinv=solve(C,diag(1,2))

# Bivariate plots of (beta1,beta2)
beta1=seq(46,65,len=100)
beta2=seq(0.5,3,len=100)
prior=function(beta1,beta2){dbvt(beta1,beta2,2*g,b,(h/g)*cinv)}
posterior=function(beta1,beta2){dbvt(beta1,beta2,2*G,B,(H/G)*Cinv)}
def.par <- par(no.readonly = TRUE) # save default, for resetting...
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
par(mar=c(3,3,1,1))
z1=outer(beta1,beta2,prior)
z2=outer(beta1,beta2,posterior)
contour(beta1,beta2,z1,xlab=expression(alpha),ylab=expression(beta),lty=3)
contour(beta1,beta2,z2,add=TRUE)
par(mar=c(0,3,1,1))
plot(beta1,dgt(beta1,2*G,B[1],(H/G)*Cinv[1,1]),type="l")
lines(beta1,dgt(beta1,2*g,b[1],(h/g)*cinv[1,1]),lty=2)
par(mar=c(3,0,1,1))
plot(dgt(beta2,2*G,B[2],(H/G)*Cinv[2,2]),beta2,type="l")
lines(dgt(beta2,2*g,b[2],(h/g)*cinv[2,2]),beta2,lty=2)
par(def.par)

# Bivariate plots of (beta1,beta2)
beta1=seq(54,57.5,len=100)
beta2=seq(1.4,1.8,len=100)
#prior=function(beta1,beta2){dbvt(beta1,beta2,2*g,b,(h/g)*cinv)}
#posterior=function(beta1,beta2){dbvt(beta1,beta2,2*G,B,(H/G)*Cinv)}
def.par <- par(no.readonly = TRUE) # save default, for resetting...
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
par(mar=c(3,3,1,1))
z1=outer(beta1,beta2,prior)
z2=outer(beta1,beta2,posterior)
contour(beta1,beta2,z1,xlab=expression(alpha),ylab=expression(beta),lty=3)
contour(beta1,beta2,z2,add=TRUE)
par(mar=c(0,3,1,1))
plot(beta1,dgt(beta1,2*G,B[1],(H/G)*Cinv[1,1]),type="l")
lines(beta1,dgt(beta1,2*g,b[1],(h/g)*cinv[1,1]),lty=2)
par(mar=c(3,0,1,1))
plot(dgt(beta2,2*G,B[2],(H/G)*Cinv[2,2]),beta2,type="l")
lines(dgt(beta2,2*g,b[2],(h/g)*cinv[2,2]),beta2,lty=2)
par(def.par)

# Plots of tau and sigma
op=par(mfrow=c(1,2))
tau=seq(0,1.5,len=1000)
plot(tau,dgamma(tau,G,H),type="l",xlab=expression(tau),ylab="density")
lines(tau,dgamma(tau,g,h),lty=2)
sigma=seq(0,3,len=1000)
plot(sigma,dinvchi(sigma,G,H),type="l",xlab=expression(sigma),ylab="density")
lines(sigma,dinvchi(sigma,g,h),type="l",lty=2)
par(op)

# 95% HDIs for the intercept alpha
# Prior
qgt(c(0.025,0.975),2*g,b[1],(h/g)*cinv[1,1])
# Posterior
qgt(c(0.025,0.975),2*G,B[1],(H/G)*Cinv[1,1])

# 95% HDIs for the slope beta
# Prior
qgt(c(0.025,0.975),2*g,b[2],(h/g)*cinv[2,2])
# Posterior
qgt(c(0.025,0.975),2*G,B[2],(H/G)*Cinv[2,2])

# 95% HDIs for tau
# Prior
hdiGamma(0.95,g,h)
# Posterior
hdiGamma(0.95,G,H)

# 95% HDIs for sigma
# Prior
hdiInvchi(0.95,g,h)
# Posterior
hdiInvchi(0.95,G,H)

quadform2d=function(beta1,beta2,b=c(0,0),c){
  as.numeric(c[1,1]*(beta1-b[1])^2+c[2,2]*(beta2-b[2])^2+2*c[1,2]*(beta1-b[1])*(beta2-b[2]))}

# 80%, 90% and 95% contour ellipses for (alpha,beta)
beta1=seq(39,73,len=100)
beta2=seq(-0.5,4,len=100)
prior=function(beta1,beta2){quadform2d(beta1,beta2,b,c)}
posterior=function(beta1,beta2){quadform2d(beta1,beta2,B,C)}
z1=outer(beta1,beta2,prior)
z2=outer(beta1,beta2,posterior)
priorlevels=2*h*c(qf(0.95,2,2*g),qf(0.9,2,2*g),qf(0.8,2,2*g))/g
priorlevels=round(priorlevels,2)
posteriorlevels=2*H*c(qf(0.95,2,2*G),qf(0.9,2,2*G),qf(0.8,2,2*G))/G
posteriorlevels=round(posteriorlevels,2)
contour(beta1,beta2,z1,levels=priorlevels,xlab=expression(alpha),ylab=expression(beta),lty=3)
contour(beta1,beta2,z2,levels=posteriorlevels,lwd=2,add=TRUE)

# 80%, 90% and 95% contour ellipses for (alpha,beta)
beta1=seq(54,57.5,len=100)
beta2=seq(1.39,1.8,len=100)
prior=function(beta1,beta2){quadform2d(beta1,beta2,b,c)}
posterior=function(beta1,beta2){quadform2d(beta1,beta2,B,C)}
z1=outer(beta1,beta2,prior)
z2=outer(beta1,beta2,posterior)
priorlevels=2*h*c(qf(0.95,2,2*g),qf(0.9,2,2*g),qf(0.8,2,2*g))/g
priorlevels=round(priorlevels,2)
posteriorlevels=2*H*c(qf(0.95,2,2*G),qf(0.9,2,2*G),qf(0.8,2,2*G))/G
posteriorlevels=round(posteriorlevels,2)
contour(beta1,beta2,z1,levels=priorlevels,xlab=expression(alpha),ylab=expression(beta),lty=3)
contour(beta1,beta2,z2,levels=posteriorlevels,lwd=2,add=TRUE)

# pointwise predictive intervals
xs=seq(min(malcolm$shoe),max(malcolm$shoe),len=100)
upper=vector("numeric",len=length(xs)); lower=vector("numeric",len=length(xs))
for (i in 1:length(xs)){upper[i]=qgt(0.975,2*G,as.numeric(t(B)%*%c(1,xs[i])),c=(quadform2d(1,xs[i],c=Cinv)+1)*H/G)}
upper=qgt(0.975,2*G,as.vector(t(B)%*%rbind(rep(1,length(xs)),xs)),c=(quadform2d(rep(1,length(xs)),xs,c=Cinv)+1)*H/G)
lower=qgt(0.025,2*G,as.vector(t(B)%*%rbind(rep(1,length(xs)),xs)),c=(quadform2d(rep(1,length(xs)),xs,c=Cinv)+1)*H/G)
plot(malcolm)

lines(xs,upper,type="l",lty=2)
lines(xs,lower,type="l",lty=2)

readline(prompt = "Ready for one way ANOVA example?")

data(coagulation)
m=lm(coag~diet-1,x=T,data=coagulation)
plot(coagulation)

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

# Marginal plots of priors and posteriors
op=par(mfrow=c(3,2))
mu1=seq(54,68,len=1000)
plot(mu1,dgt(mu1,2*G,B[1],(H/G)*Cinv[1,1]),type="l",xlab=expression(mu[1]),ylab="density")
lines(mu1,dgt(mu1,2*g,b[1],(h/g)*cinv[1,1]),lty=2)
mu2=seq(54,68,len=1000)
plot(mu2,dgt(mu2,2*G,B[2],(H/G)*Cinv[2,2]),type="l",xlab=expression(mu[2]),ylab="density")
lines(mu2,dgt(mu2,2*g,b[2],(h/g)*cinv[2,2]),lty=2)
mu3=seq(54,68,len=1000)
plot(mu3,dgt(mu3,2*G,B[3],(H/G)*Cinv[3,3]),type="l",xlab=expression(mu[3]),ylab="density")
lines(mu3,dgt(mu3,2*g,b[3],(h/g)*cinv[3,3]),lty=2)
plot.new()
tau=seq(0,0.4,len=1000)
plot(tau,dgamma(tau,G,H),type="l",xlab=expression(tau),ylab="density")
lines(tau,dgamma(tau,g,h),lty=2)
sigma=seq(0,10,len=1000)
plot(sigma,dinvchi(sigma,G,H),type="l",xlab=expression(sigma),ylab="density")
lines(sigma,dinvchi(sigma,g,h),type="l",lty=2)
par(op)

A=matrix(c(-1,-1,1,0,0,1),ncol=3)
bs=A%*%b
cs=solve(A%*%c%*%t(A),diag(1,2))
csinv=solve(cs,diag(1,2))
Bs=A%*%B
Cs=solve(A%*%Cinv%*%t(A),diag(1,2))
Csinv=solve(Cs,diag(1,2))

# Bivariate plots of (mu2-mu1,mu3-mu1)
alpha2=seq(-10,10,len=100)
alpha3=seq(-10,10,len=100)
prior=function(beta1,beta2){dbvt(beta1,beta2,2*g,bs,(h/g)*csinv)}
posterior=function(beta1,beta2){dbvt(beta1,beta2,2*G,Bs,(H/G)*Csinv)}
def.par <- par(no.readonly = TRUE) # save default, for resetting...
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
par(mar=c(3,3,1,1))
z1=outer(alpha2,alpha3,prior)
z2=outer(alpha2,alpha3,posterior)
contour(alpha2,alpha3,z1,xlab=expression(mu[2]-mu[1]),ylab=expression(mu[3]-mu[1]),lty=3)
contour(alpha2,alpha3,z2,add=TRUE)
par(mar=c(0,3,1,1))
plot(alpha2,dgt(alpha2,2*G,Bs[1],(H/G)*Csinv[1,1]),type="l")
lines(alpha2,dgt(alpha2,2*g,bs[1],(h/g)*csinv[1,1]),lty=2)
par(mar=c(3,0,1,1))
plot(dgt(alpha3,2*G,Bs[2],(H/G)*Csinv[2,2]),alpha3,type="l")
lines(dgt(alpha3,2*g,bs[2],(h/g)*csinv[2,2]),alpha3,lty=2)
par(def.par)

# 80%, 90% and 95% contour ellipses for (mu2-mu1,mu3-mu1)
alpha2=seq(-40,40,len=100)
alpha3=seq(-40,40,len=100)
prior=function(beta1,beta2){quadform2d(beta1,beta2,bs,cs)}
posterior=function(beta1,beta2){quadform2d(beta1,beta2,Bs,Cs)}
z1=outer(alpha2,alpha3,prior)
z2=outer(alpha2,alpha3,posterior)
priorlevels=2*h*c(qf(0.95,2,2*g),qf(0.9,2,2*g),qf(0.8,2,2*g))/g
priorlevels=round(priorlevels,2)
posteriorlevels=2*H*c(qf(0.95,2,2*G),qf(0.9,2,2*G),qf(0.8,2,2*G))/G
posteriorlevels=round(posteriorlevels,2)
contour(alpha2,alpha3,z1,levels=priorlevels,xlab=expression(mu[2]-mu[1]),ylab=expression(mu[3]-mu[1]),lty=3)
contour(alpha2,alpha3,z2,levels=posteriorlevels,lwd=2,add=T)

alpha2=seq(-2,6,len=100)
alpha3=seq(-4,4,len=100)
z1=outer(alpha2,alpha3,prior)
z2=outer(alpha2,alpha3,posterior)
priorlevels=2*h*c(qf(0.95,2,2*g),qf(0.9,2,2*g),qf(0.8,2,2*g))/g
priorlevels=round(priorlevels,2)
posteriorlevels=2*H*c(qf(0.95,2,2*G),qf(0.9,2,2*G),qf(0.8,2,2*G))/G
posteriorlevels=round(posteriorlevels,2)
contour(alpha2,alpha3,z1,levels=priorlevels,xlab=expression(mu[2]-mu[1]),ylab=expression(mu[3]-mu[1]),lty=3)
contour(alpha2,alpha3,z2,levels=posteriorlevels,lwd=2,add=T)


