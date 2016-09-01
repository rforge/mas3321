# Fig 2.1
xgrid=seq(-3,3,len=100)
ygrid=seq(-3,3,len=100)
rho=0.5
z = outer(xgrid, ygrid, dbvnorm, c(0,0), matrix(c(1,rho,rho,1),ncol=2))
persp(xgrid,ygrid,z,theta=10,phi=10,xlab=NULL,ylab="",zlab="",ticktype="detailed")

# Fig 2.2
def.par <- par(no.readonly = TRUE) # save default, for resetting...
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
par(mar=c(3,3,1,1))
contour(xgrid,ygrid,z)
lines(c(-2,2),c(-2,2))
lines(c(-1.3,1.3),c(1.3,-1.3))
par(mar=c(0,3,1,1))
plot(xgrid,dnorm(xgrid),type="l")
par(mar=c(3,0,1,1))
plot(dnorm(ygrid),ygrid,type="l")
par(def.par)

