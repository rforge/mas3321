# analyses using a Gibbs sampler

posterior=gibbsNormal(N=11000,initial=c(10,0.25),b=10,c=1/100,g=3,h=12,n=100,xbar=15,s=4.5)
posterior2=mcmcProcess(input=posterior)

# Fig 4.1
op=par(mfrow=c(2,2))
plot(posterior,col=1:10000,main="All realisations")
plot(posterior,type="l",main="All realisations")
plot(posterior2,col=1:10000,main="After deleting first 1000")
plot(posterior2,type="l",main="After deleting first 1000")
par(op)


# Fig 4.2
mcmcAnalysis(posterior,rows=2,show=F)

mcmcAnalysis(posterior2,rows=2,show=F)

# Fig 4.3
mu=posterior2[,1]; tau=posterior2[,2]
xhist <- hist(mu, breaks=30,,plot=FALSE)
yhist <- hist(tau, breaks=30, plot=FALSE)
top <- max(c(xhist$counts, yhist$counts))
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
par(mar=c(5,5,1,1))
plot(mu,tau, xlab=expression(mu), ylab=expression(tau))
par(mar=c(0,3,1,1))
barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0,density=0)
par(mar=c(3,0,1,1))
barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0,density=0, horiz=TRUE)

mu=posterior2[,1]; sigma=1/sqrt(posterior2[,2])
xhist <- hist(mu, breaks=30,,plot=FALSE)
yhist <- hist(sigma, breaks=30, plot=FALSE)
top <- max(c(xhist$counts, yhist$counts))
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
par(mar=c(5,5,1,1))
plot(mu,sigma, xlab=expression(mu), ylab=expression(sigma))
par(mar=c(0,3,1,1))
barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0,density=0)
par(mar=c(3,0,1,1))
barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0,density=0, horiz=TRUE)

mcmcCi(posterior2)

#########################################################################

posterior=gibbsNormal2(N=10010,initial=c(5.41,25),b=5.41,c=0.25,g=2.5,h=0.1,n=23,xbar=5.4848,s=0.1882)
posterior2=mcmcProcess(input=posterior,burnin=10)

# Fig 4.4
mcmcAnalysis(posterior,rows=2,show=F)
mcmcAnalysis(posterior2,rows=2,show=F)

# Fig 4.5
mu=seq(5.3,5.7,len=1000)
hist(posterior[11:10010,1],xlab=expression(mu),main="",freq=F,breaks=30)
lines(mu,dgt(mu,28,5.4840,0.001561))

tau=seq(10,60,len=1000)
hist(posterior[11:10010,2],xlab=expression(tau),main="",freq=F,breaks=30)
lines(tau,dgamma(tau,14,0.5080))
