# Metropolis-Hastings and Metropolis within Gibbs samplers
posterior=metropolis(N=10000,initial=0,a=0.5)

# Fig 4.6
mcmcAnalysis(posterior,rows=1,show=F)

posterior=metropolis(N=10000,initial=0,a=5)

mcmcAnalysis(posterior,rows=1,show=F)

posterior=metropolis(N=10000,initial=0,a=50)

mcmcAnalysis(posterior,rows=1,show=F)

##############################################################

posterior=mwgGamma(N=20000,initial=(0.62/0.4)^2,innov=0.9,priorparam=c(2,1,3,1),n=50,xbar=0.62,xgbar=0.46,show=TRUE) 

# Fig 4.7
mcmcAnalysis(posterior,rows=2,show=F)  

posterior2=mcmcProcess(input=posterior,burnin=10,thin=20)

mcmcAnalysis(posterior2,rows=2)

