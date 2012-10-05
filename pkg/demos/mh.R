posterior=metrop(N=10000,initial=0,a=0.5)
post=as.matrix(posterior);colnames(post)="theta"

#mys3pdf("mhnorm1")
mcmcSummary(post,rows=1,show=F)
#dev.off()

posterior=metrop(N=10000,initial=0,a=5)
post=as.matrix(posterior);colnames(post)="theta"

#mys3pdf("mhnorm2")
mcmcSummary(post,rows=1,show=F)
#dev.off()

posterior=metrop(N=10000,initial=0,a=50)
post=as.matrix(posterior);colnames(post)="theta"

#mys3pdf("mhnorm3")
mcmcSummary(post,rows=1,show=F)
#dev.off()


ybar=c(131.23,316.5,176.25,283.25,72,594.75,15.75,336,298.5,43)
s=c(61.68,127.3,124.47,235.76,57.32,390.2,12.93,286.9,262.36,30.67)
n=c(4,4,4,4,4,4,4,4,4,4)

intau=1e-5; innu=7.9
posterior=mh(N=10000,initial=c(200,2e-5,1),intau=intau,innu=innu,a=200,b=0.1,c=0.1,d=0.1,e=0.1,f=0.1,m=10,n=n,ybar=ybar,s=s,show=TRUE)

#pdf("mhnrw.pdf",height=5.5,width=6);par(mar=c(5,4,1,2)+0.1)
mcmcSummary(posterior,rows=3)
#dev.off()

intau=1e-5; innu=2
posterior=mh(N=10000,initial=c(200,2e-5,1),intau=intau,innu=innu,a=200,b=0.1,c=0.1,d=0.1,e=0.1,f=0.1,m=10,n=n,ybar=ybar,s=s,show=TRUE)
mcmcSummary(posterior,rows=3)

posterior2=mcmcProcess(input=posterior,burnin=1,thin=10)

mcmcSummary(posterior2,rows=3)


ybar=c(131.23,316.5,176.25,283.25,72,594.75,15.75,336,298.5,43)
s=c(61.68,127.3,124.47,235.76,57.32,390.2,12.93,286.9,262.36,30.67)
n=c(4,4,4,4,4,4,4,4,4,4)

intau=0.7; innu=12
posterior=mh2(N=100000,initial=c(200,2e-5,1),intau=intau,innu=innu,a=200,b=0.1,c=0.1,d=0.1,e=0.1,f=0.1,m=10,n=n,ybar=ybar,s=s,show=TRUE)
mcmcSummary(posterior,rows=3)

posterior2=mcmcProcess(input=posterior,burnin=1,thin=10)

#pdf("mhnrwl.pdf",height=5.5,width=6);par(mar=c(5,4,1,2)+0.1)
mcmcSummary(posterior2,rows=3)
#dev.off()

