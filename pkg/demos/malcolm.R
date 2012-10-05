#x=round(2*rnorm(152,8,2))/2;y=round(rnorm(length(x),56.2+1.56*x,sqrt(2)))
#fred=data.frame(x=x,y=y)
malcolm=read.table("malcolm.dat",header=T)
m=lm(y~x,x=T,data=malcolm)
betahat=m$coefficients
XtX=t(m$x)%*%m$x
RSS=sum(residuals(m)^2)

b=c(167/3,5/3)
c=4*solve(matrix(c(36.5,-4,-4,0.5),ncol=2),diag(1,2))
g=2
h=4

B=solve(c+XtX,c%*%b+XtX%*%betahat)
C=c+XtX
G=g+length(malcolm$y)/2
H=as.numeric(h+0.5*(RSS-t(B)%*%(c+XtX)%*%B+t(b)%*%c%*%b+t(betahat)%*%XtX%*%betahat))
cinv=solve(c,diag(1,2))
Cinv=solve(C,diag(1,2))

# prior and posterior variance matrices
(h/(g-1))*cinv
(H/(G-1))*Cinv
# prior and posterior correlations
cinv[1,2]/sqrt(cinv[1,1]*cinv[2,2])
Cinv[1,2]/sqrt(Cinv[1,1]*Cinv[2,2])


# Fig 
#myspdf("malcolm-post-beta1")
beta1=seq(45,65,len=1000)
plot(beta1,dgt(beta1,2*G,B[1],(H/G)*Cinv[1,1]),type="l",xlab=expression(alpha),ylab="density")
lines(beta1,dgt(beta1,2*g,b[1],(h/g)*cinv[1,1]),lty=2)
#dev.off()

#myspdf("malcolm-post-beta2")
beta2=seq(0,3,len=1000)
plot(beta2,dgt(beta2,2*G,B[2],(H/G)*Cinv[2,2]),type="l",xlab=expression(beta),ylab="density")
lines(beta2,dgt(beta2,2*g,b[2],(h/g)*cinv[2,2]),lty=2)
#dev.off()

#myspdf("malcolm-post-tau")
tau=seq(0,1.5,len=1000)
plot(tau,dgamma(tau,G,H),type="l",xlab=expression(tau),ylab="density")
lines(tau,dgamma(tau,g,h),lty=2)
#dev.off()


# don't put this in the notes!!
#myspdf("malcolm-post-sigma")
sigma=seq(0,3,len=1000)
plot(sigma,dinvchi(sigma,G,H),type="l",xlab=expression(sigma),ylab="density")
lines(sigma,dinvchi(sigma,g,h),type="l",lty=2)
#dev.off()


#mys4pdf("malcolm-beta-joint1")
beta1grid=seq(46,65,len=100)
beta2grid=seq(0.5,3,len=100)
prior=function(beta1,beta2){dbvt(beta1,beta2,2*g,b,(h/g)*cinv)}
posterior=function(beta1,beta2){dbvt(beta1,beta2,2*G,B,(H/G)*Cinv)}
z1=outer(beta1grid,beta2grid,prior)
z2=outer(beta1grid,beta2grid,posterior)
contour(beta1grid,beta2grid,z1,xlab=expression(alpha),ylab=expression(beta),lty=3)
contour(beta1grid,beta2grid,z2,add=T)
#dev.off()

#mys4pdf("malcolm-beta-joint2")
beta1grid=seq(54,57.5,len=100)
beta2grid=seq(1.4,1.8,len=100)
prior=function(beta1,beta2){dbvt(beta1,beta2,2*g,b,(h/g)*cinv)}
posterior=function(beta1,beta2){dbvt(beta1,beta2,2*G,B,(H/G)*Cinv)}
z1=outer(beta1grid,beta2grid,prior)
z2=outer(beta1grid,beta2grid,posterior)
contour(beta1grid,beta2grid,z1,xlab=expression(alpha),ylab=expression(beta),lty=3)
contour(beta1grid,beta2grid,z2,add=T)
#dev.off()

# 95% HDI bounds for alpha and beta
c(B[1]-qt(0.975,2*G)*sqrt(H*Cinv[1,1]/G),B[1]+qt(0.975,2*G)*sqrt(H*Cinv[1,1]/G))
c(B[2]-qt(0.975,2*G)*sqrt(H*Cinv[2,2]/G),B[2]+qt(0.975,2*G)*sqrt(H*Cinv[2,2]/G))

# 95% HDI bounds for tau and sigma
hdiGamma=(0.95,G,H)

hdiInvchi=(0.95,G,H)

# 95% equi-tailed CI for tau and sigma
qgamma(0.025,G,H);qgamma(0.975,G,H)

1/sqrt(qgamma(0.975,G,H));1/sqrt(qgamma(0.025,G,H))


#mys4pdf("malcolm-beta-conf1")
beta1grid=seq(39,73,len=100)
beta2grid=seq(-0.5,4,len=100)
prior=function(beta1,beta2){quadform2d(beta1,beta2,b,c)}
posterior=function(beta1,beta2){quadform2d(beta1,beta2,B,C)}
z1=outer(beta1grid,beta2grid,prior)
z2=outer(beta1grid,beta2grid,posterior)
priorlevels=2*h*c(qf(0.95,2,2*g),qf(0.9,2,2*g),qf(0.8,2,2*g))/g
priorlevels=round(priorlevels,2)
posteriorlevels=2*H*c(qf(0.95,2,2*G),qf(0.9,2,2*G),qf(0.8,2,2*G))/G
posteriorlevels=round(posteriorlevels,2)
contour(beta1grid,beta2grid,z1,levels=priorlevels,xlab=expression(alpha),ylab=expression(beta),lty=3)
contour(beta1grid,beta2grid,z2,levels=posteriorlevels,lwd=2,add=T)
#dev.off()


#mys4pdf("malcolm-beta-conf2")
beta1grid=seq(54,57.5,len=100)
beta2grid=seq(1.39,1.8,len=100)
prior=function(beta1,beta2){quadform2d(beta1,beta2,b,c)}
posterior=function(beta1,beta2){quadform2d(beta1,beta2,B,C)}
z1=outer(beta1grid,beta2grid,prior)
z2=outer(beta1grid,beta2grid,posterior)
priorlevels=2*h*c(qf(0.95,2,2*g),qf(0.9,2,2*g),qf(0.8,2,2*g))/g
priorlevels=round(priorlevels,2)
posteriorlevels=2*H*c(qf(0.95,2,2*G),qf(0.9,2,2*G),qf(0.8,2,2*G))/G
posteriorlevels=round(posteriorlevels,2)
contour(beta1grid,beta2grid,z1,levels=priorlevels,xlab=expression(alpha),ylab=expression(beta),lty=3)
contour(beta1grid,beta2grid,z2,levels=posteriorlevels,add=T)
#dev.off()


# simulation verification of confidence level
n=10000
k=2*h*qf(0.95,2,2*g)/g
V=h*cinv/g
rho=V[1,2]/sqrt(V[1,1]*V[2,2]);
xs=rt(n,2*g);ys=rho*xs+sqrt((1-rho*rho)*(2*g+xs^2)/(2*g+1))*rt(n,2*g+1);
x=b[1]+sqrt(V[1,1])*xs;y=b[2]+sqrt(V[2,2])*ys
mean(quadform2d(x,y,b,c)<k)

