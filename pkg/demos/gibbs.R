posterior=gibbsnorm(N=11000,initial=c(10,0.25),b=10,c=1/100,g=3,h=12,n=100,xbar=15,s=4.5)
posterior2=mcmcProcess(input=posterior)

#mypdf("gibbsnorm1")
op=par(mfrow=c(2,2))
plot(posterior,col=1:10000,main="All realisations")
plot(posterior,type="l",main="All realisations")
plot(posterior2,col=1:10000,main="After deleting first 1000")
plot(posterior2,type="l",main="After deleting first 1000")
par(op)
#dev.off()

#mypdf("gibbsnorm2")
mcmcSummary(posterior,rows=2,show=F)
#dev.off()

#mypdf("gibbsnorm3")
mcmcSummary(posterior2,rows=2,show=F)
#dev.off()

#pdf("gibbsnorm3a.pdf",height=4.5,width=4.5)
mu=posterior2[,1]; tau=posterior2[,2]
xhist <- hist(mu, breaks=30,,plot=FALSE)
yhist <- hist(tau, breaks=30, plot=FALSE)
top <- max(c(xhist$counts, yhist$counts))
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
# layout.show(nf)
par(mar=c(5,5,1,1))
#par(mar=c(3,3,1,1))
plot(mu,tau, xlab=expression(mu), ylab=expression(tau))
par(mar=c(0,3,1,1))
barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0,density=0)
par(mar=c(3,0,1,1))
barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0,density=0, horiz=TRUE)
#dev.off()

pdf("gibbsnorm3b.pdf",height=4.5,width=4.5)
mu=posterior2[,1]; sigma=1/sqrt(posterior2[,2])
xhist <- hist(mu, breaks=30,,plot=FALSE)
yhist <- hist(sigma, breaks=30, plot=FALSE)
top <- max(c(xhist$counts, yhist$counts))
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
# layout.show(nf)
par(mar=c(5,5,1,1))
#par(mar=c(3,3,1,1))
plot(mu,sigma, xlab=expression(mu), ylab=expression(sigma))
par(mar=c(0,3,1,1))
barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0,density=0)
par(mar=c(3,0,1,1))
barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0,density=0, horiz=TRUE)
#dev.off()

mu=posterior2[,1]; tau=posterior2[,2]; sigma=1/sqrt(posterior2[,2])
mu=sort(mu); tau=sort(tau); sigma=sort(sigma)
N=10000; al=0.05
c(mu[floor(N*al/2)],mu[floor(N*(1-al/2))])
c(tau[floor(N*al/2)],tau[floor(N*(1-al/2))])
c(sigma[floor(N*al/2)],sigma[floor(N*(1-al/2))])

#########################################################################

posterior=gibbsnorm2(N=10010,initial=c(5.41,25),b=5.41,c=0.25,g=2.5,h=0.1,n=23,xbar=5.4848,s=0.1882)
posterior2=mcmcProcess(input=posterior,burnin=10)

op=par(mfrow=c(2,2))
plot(posterior,col=1:10010,main="All realisations")
plot(posterior,type="l",main="All realisations")
plot(posterior2,col=1:10000,main="After deleting first 10")
plot(posterior2,type="l",main="After deleting first 10")
par(op)

mcmcSummary(posterior,rows=2,show=F)
mcmcSummary(posterior2,rows=2,show=F)


mypdf("gibbsnorm4")
mcmcSummary(posterior[11:10010,],rows=2,show=F)
dev.off()

#myspdf("gibbsnorm5")
mu=seq(5.3,5.7,len=1000)
hist(posterior[11:10010,1],xlab=expression(mu),main="",freq=F,breaks=30)
lines(mu,dgt(mu,28,5.4840,0.001561))
#dev.off()

#myspdf("gibbsnorm6")
tau=seq(10,60,len=1000)
hist(posterior[11:10010,2],xlab=expression(tau),main="",freq=F,breaks=30)
lines(tau,dgamma(tau,14,0.5080))
#dev.off()

###################################################################

keyboarddata=read.table("keyboard.dat",header=T)
m=lm(acc~keyboard-1,data=keyboarddata)

keymeans=c(131.25,43,316.5,176.25,283.25,72,594.75,15.75,366,298.5)
summary(keymeans)
sd(keymeans)
y=c(170,210,55,90);c(mean(y),sd(y)*sqrt(3/4))
y=c(437,450,200,179);c(mean(y),sd(y)*sqrt(3/4))
y=c(210,350,5,140);c(mean(y),sd(y)*sqrt(3/4))
y=c(560,470,10,93);c(mean(y),sd(y)*sqrt(3/4))
y=c(47,166,12,63);c(mean(y),sd(y)*sqrt(3/4))
y=c(921,1043,237,178);c(mean(y),sd(y)*sqrt(3/4))
y=c(34,21,0,8);c(mean(y),sd(y)*sqrt(3/4))
y=c(585,658,34,67);c(mean(y),sd(y)*sqrt(3/4))
y=c(647,457,34,56);c(mean(y),sd(y)*sqrt(3/4))
y=c(78,67,24,3);c(mean(y),sd(y)*sqrt(3/4))

ybar=c(131.23,316.5,176.25,283.25,72,594.75,15.75,336,298.5,43)
s=c(61.68,127.3,124.47,235.76,57.32,390.2,12.93,286.9,262.36,30.67)
n=c(4,4,4,4,4,4,4,4,4,4)

posterior=gibbsnorm3(N=10000,initial=c(200,2e-5,6e-3,ybar),a=200,b=0.1,c=0.1,d=0.1,e=0.1,f=0.1,m=10,n=n,ybar=ybar,s=s)

#mys2pdf("gibbsnorm7")
mcmcSummary(posterior[,1:5],rows=5)
#dev.off()

mcmcSummary(posterior[,6:10],rows=5)

mcmcSummary(posterior[,11:13],rows=5)



posterior=gibbsnorm3(N=1000000,initial=c(200,2e-5,6e-3,ybar),a=200,b=0.1,c=0.1,d=0.1,e=0.1,f=0.1,m=10,n=n,ybar=ybar,s=s)
posterior2=mcmcProcess(input=posterior,burnin=500000,thin=50)

#mys2pdf("gibbsnorm8")
mcmcSummary(posterior2[,1:5],rows=5)
#dev.off()

#mys2pdf("gibbsnorm9")
mcmcSummary(posterior2[,6:10],rows=5)
#dev.off()

#pdf("gibbsnorm10.pdf",height=5.5,width=6);par(mar=c(5,4,1,2)+0.1)
mcmcSummary(posterior2[,11:13],rows=3)
#dev.off()


# boxplot function
mcmcBoxplot(posterior2,4,13)

# heat map of correlations
#pdf("gibbsnorm11.pdf",height=6,width=6);par(mar=c(5,4,1,2)+0.1)
heatmap.2(cor(posterior2),symm=TRUE, margins=c(6,6),Colv = NA,Rowv = NA,dendrogram = "none",symbreaks=TRUE,trace="none")
#dev.off()

# CIs

mcmcCi(posterior2,0.95)
