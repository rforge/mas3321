#' Metropolis algorithm to simulate realisations from a standard normal distribution
#' 
#' Metropolis algorithm to simulate realisations from a standard normal distribution
#' using a U(-a,a) random walk proposal
#' @param N length of MCMC chain
#' @param initial starting value for the algorithm
#' @param a size of uniform innovations
#' @param show logical. If true then acceptance rate for the proposals will be given
#' @export metropolis
#' @examples mcmcAnalysis(metropolis(100,0,1),rows=1)
metropolis=function(N,initial,a,show=TRUE) 
{
  output=matrix(ncol=1,nrow=N)
  colnames(output)="theta"
  x=initial
  output[1]=x
  naccept=0
  for (i in 2:N) {
    proposal=x+runif(1,-a,a)
    aprob=dnorm(proposal)/dnorm(x)
    u=runif(1)
    if (u<aprob) {
        x=proposal
        naccept=naccept+1
    }
    output[i,]=x
  }
  if (show==TRUE) message(paste("Acceptance rate = ",naccept/N))
  output
}

#' Metropolis within Gibbs algorithm for a gamma random sample
#' 
#' Simulates realisations from the posterior distribution for the index and shape parameters in a
#' gamma distribution based on a random sample and independent gamma priors by using a Metropolis 
#' within Gibbs algorithm and a normal random walk proposal for the index parameter
#' @param N length of MCMC chain
#' @param initial starting value for the algorithm
#' @param innov standard deviation of normal random walk innovation for index parameter
#' @param priorparam prior parameters a,b,c,d
#' @param n size of random sample
#' @param xbar (arithmetic) mean of random sample
#' @param xgbar geometric mean of random sample
#' @param show logical. If true then acceptance rate for the proposals will be given
#' @export mwgGamma
#' @examples mcmcAnalysis(mwgGamma(100,(0.62/0.4)^2,0.8,c(2,1,3,1),50,0.62,0.46),rows=2)
mwgGamma=function(N,initial,innov,priorparam,n,xbar,xgbar,show=TRUE) 
{
  output=matrix(ncol=2,nrow=N)
  colnames(output)=c("alpha","lambda")
  a=priorparam[1];b=priorparam[2];c=priorparam[3];d=priorparam[4];
  alpha=initial
  naccept=0
  for (i in 1:N) {
    lambda=rgamma(1,c+n*alpha,d+n*xbar)
    proposal=rnorm(1,alpha,innov)
    if (proposal>0) {
    logaprob=(a-1)*log(proposal/alpha)+n*log(gamma(alpha)/gamma(proposal))+
      (b+n*log(xgbar)+n*log(lambda))*(proposal-alpha)
    u=runif(1)
    if (log(u)<logaprob) {
      alpha=proposal
      naccept=naccept+1
    }}
    output[i,]=c(alpha,lambda)
  }
  if (show==TRUE) message(paste("Acceptance rate = ",naccept/N))
  output
}

#' Metropolis-Hastings algorithm for a one-way normal random effects model
#' 
#' Simulates realisations from the posterior distribution for the population mean  
#' and precision components in a one-way normal random effects model with a semi-conjugate prior.
#' The method marginalises over the random effects and uses univariate normal or log normal random 
#' walk proposals for the precision components.
#' @param N length of MCMC chain
#' @param initial starting values for the algorithm
#' @param intau standard deviation of normal random walk innovation for data precision parameter tau
#' @param innu standard deviation of normal random walk innovation for random effects precision parameter nu
#' @param priorparam prior parameters a,b,c,d,e,f
#' @param m number of treatments
#' @param n vector containing the number of observations on each treatment
#' @param ybar vector containing the mean of observations on each treatment
#' @param s vector containing the standard deviation of observations on each treatment
#' @param show logical. If true then acceptance rate for the proposals will be given
#' @param innLogscale logical. If true then proposals are made on a log scale
#' @export mhReffects
#' @examples 
#' data(contamination)
#' n=tapply(contamination$acc,contamination$keyboard,length)
#' ybar=tapply(contamination$acc,contamination$keyboard,mean)
#' s=sqrt(tapply(contamination$acc,contamination$keyboard,var)*(n-1)/n)
#' mcmcAnalysis(mhReffects(N=100,initial=c(200,2e-5,1),intau=1e-5,innu=7.9,priorparam=c(200,0.1,0.1,0.1,0.1,0.1),m=10,n=n,ybar=ybar,s=s,show=TRUE),rows=3)
mhReffects=function(N,initial,intau,innu,priorparam,m,n,ybar,s,show=TRUE,innLogscale=FALSE) 
{
# log FCDs used my mhReffects  
  lfcdtau=function(tau,mu,nu,c,d,m,n,ybar,s)
  {(c+sum(n)/2-1)*log(tau)-d*tau-0.5*sum(log(nu+n*tau))-
     0.5*tau*sum(n*s^2)-0.5*tau*nu*sum(n*(ybar-mu)^2/(nu+n*tau))}
  lfcdnu=function(nu,mu,tau,e,f,m,n,ybar)
  {(e+m/2-1)*log(nu)-f*nu-0.5*sum(log(nu+n*tau))-
     0.5*tau*nu*sum(n*(ybar-mu)^2/(nu+n*tau))}
#  
  a=priorparam[1];b=priorparam[2];c=priorparam[3];d=priorparam[4];e=priorparam[5];f=priorparam[6];
  output=matrix(ncol=3,nrow=N)
  colnames(output)=c("mu","tau","nu")
  mu=initial[1]; tau=initial[2]; nu=initial[3]
  output[1,]=c(mu,tau,nu)
  naccepttau=0; nacceptnu=0
  for (i in 2:N) {
    muprec=b+tau*nu*sum(n/(nu+n*tau))
    mumean=(a*b+tau*nu*sum(n*ybar/(nu+n*tau)))/muprec
    mu=rnorm(1,mumean,1/sqrt(muprec))
    if (innLogscale==FALSE) {proptau=rnorm(1,tau,intau)}
      else {proptau=exp(rnorm(1,log(tau),intau))}
    if (proptau>0) {
       alprob=lfcdtau(proptau,mu,nu,c,d,m,n,ybar,s)-
                    lfcdtau(tau,mu,nu,c,d,m,n,ybar,s)
       if (innLogscale==TRUE) {alprob=alprob+log(proptau)-log(tau)}
       lu=log(runif(1))
       if (lu<alprob) {
          tau=proptau
          naccepttau=naccepttau+1
       }
    }
    if (innLogscale==FALSE) {propnu=rnorm(1,nu,innu)}
    else {propnu=exp(rnorm(1,log(nu),innu))}
    if (propnu>0) {
      alprob=lfcdnu(propnu,mu,tau,c,d,m,n,ybar)-
                  lfcdnu(nu,mu,tau,c,d,m,n,ybar)
      if (innLogscale==TRUE) {alprob=alprob+log(propnu)-log(nu)}
      lu=log(runif(1))
      if (lu<alprob) {
        nu=propnu
        nacceptnu=nacceptnu+1
      }
    }
    output[i,]=c(mu,tau,nu)
  }
  if (show==TRUE) {
     message(paste("Acceptance rate for tau = ",naccepttau/N))
     message(paste("Acceptance rate for nu = ",nacceptnu/N))
  }
  output
}
