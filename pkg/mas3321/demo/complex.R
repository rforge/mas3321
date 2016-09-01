# analyses of the one-way random effects model
data(contamination)
n=tapply(contamination$acc,contamination$keyboard,length)
ybar=tapply(contamination$acc,contamination$keyboard,mean)
s=sqrt(tapply(contamination$acc,contamination$keyboard,var)*(n-1)/n)
m=length(unique(contamination$keyboard))

posterior=gibbsReffects(N=10000,initial=c(200,2e-5,6e-3),priorparam=c(200,0.1,0.1,0.1,0.1,0.1),m=m,n=n,ybar=ybar,s=s)

# Fig 5.1
mcmcAnalysis(posterior[,1:5],rows=5)

#mcmcAnalysis(posterior[,6:10],rows=5)

#mcmcAnalysis(posterior[,11:13],rows=5)

# longer run
posterior=gibbsReffects(N=1000000,initial=c(200,2e-5,6e-3),priorparam=c(200,0.1,0.1,0.1,0.1,0.1),m=m,n=n,ybar=ybar,s=s)
posterior2=mcmcProcess(input=posterior,burnin=500000,thin=50)

# Figs 5.2-5.4
mcmcAnalysis(posterior2[,1:5],rows=5)

mcmcAnalysis(posterior2[,6:10],rows=5)

mcmcAnalysis(posterior2[,11:13],rows=3)

mcmcCi(posterior2)

# boxplot function
mcmcBoxplot(posterior2[,4:13])

# timing
system.time(gibbsReffects(N=1000000,initial=c(200,2e-5,1),priorparam=c(200,0.1,0.1,0.1,0.1,0.1),m=10,n=n,ybar=ybar,s=s))

# CIs

mcmcCi(posterior2,0.95)

#########################################################################

data(contamination)
n=tapply(contamination$acc,contamination$keyboard,length)
ybar=tapply(contamination$acc,contamination$keyboard,mean)
s=sqrt(tapply(contamination$acc,contamination$keyboard,var)*(n-1)/n)
m=length(unique(contamination$keyboard))

posterior=mhReffects(N=10000,initial=c(200,2e-5,1),intau=1e-5,innu=7.9,priorparam=c(200,0.1,0.1,0.1,0.1,0.1),m=m,n=n,ybar=ybar,s=s,show=TRUE)

# Fig 5.6
mcmcAnalysis(posterior,rows=3)

# longer run
posterior=mhReffects(N=100000,initial=c(200,2e-5,1),intau=0.7,innu=12,priorparam=c(200,0.1,0.1,0.1,0.1,0.1),m=m,n=n,ybar=ybar,s=s,show=TRUE,innLogscale=TRUE)
posterior2=mcmcProcess(input=posterior,burnin=1,thin=10)

# Fig 5.7
mcmcAnalysis(posterior2,rows=3)

# timing
system.time(mhReffects(N=100000,initial=c(200,2e-5,1),intau=0.7,innu=12,priorparam=c(200,0.1,0.1,0.1,0.1,0.1),m=m,n=n,ybar=ybar,s=s,show=TRUE,innLogscale=TRUE))
