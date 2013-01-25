#' Gibbs sampler for a normal random sample with a semi-conjugate prior
#' 
#' Simulates realisations from the posterior distribution for the mean and precision in a
#' normal distribution based on a random sample and a semi-conjugate prior by using a Gibbs sampler
#' @param N length of MCMC chain
#' @param initial starting value for the algorithm
#' @param priorparam prior parameters b,c,g,h
#' @param n size of random sample
#' @param xbar mean of random sample
#' @param s standard deviation of random sample
#' @export gibbsNormal
#' @examples mcmcAnalysis(gibbsNormal(N=100,initial=c(10,0.25),priorparam=c(10,1/100,3,12),n=100,xbar=15,s=4.5),rows=2)
gibbsNormal=function(N,initial,priorparam,n,xbar,s)
{
  b=priorparam[1];c=priorparam[2];g=priorparam[3];h=priorparam[4]
  output=matrix(ncol=2,nrow=N)
    colnames(output)=c("mu","tau")
    mu=initial[1]; tau=initial[2]
    output[1,]=c(mu,tau)
    for (i in 2:N) {
           muprec=c+n*tau
           mumean=(b*c+n*tau*xbar)/muprec
           mu=rnorm(1,mumean,1/sqrt(muprec))
           tauh=h+0.5*n*(s^2+(xbar-mu)^2)
           tau=rgamma(1,g+n/2,tauh)
           output[i,]=c(mu,tau)
    }
    output
}


#' Gibbs sampler for a normal random sample with a conjugate prior
#' 
#' Simulates realisations from the posterior distribution for the mean and precision in a
#' normal distribution based on a random sample and a conjugate normal-gamma prior distribution 
#' by using a Gibbs sampler
#' @param N length of MCMC chain
#' @param initial starting value for the algorithm
#' @param priorparam prior parameters b,c,g,h
#' @param n size of random sample
#' @param xbar mean of random sample
#' @param s standard deviation of random sample
#' @export gibbsNormal2
#' @examples mcmcAnalysis(gibbsNormal2(N=100,initial=c(5.41,25),priorparam=c(5.41,0.25,2.5,0.1),n=23,xbar=5.4848,s=0.1882),rows=2)
gibbsNormal2=function(N,initial,priorparam,n,xbar,s)
{
  b=priorparam[1];c=priorparam[2];g=priorparam[3];h=priorparam[4]
  output=matrix(ncol=2,nrow=N)
    colnames(output)=c("mu","tau")
    B=(b*c+n*xbar)/(c+n); C=c+n
    G=g+n/2; H=h+c*n*(xbar-b)^2/(2*(c+n))+n*s^2/2
    mu=initial[1]; tau=initial[2]
    output[1,]=c(mu,tau)
    for (i in 2:N) {
           mu=rnorm(1,B,1/sqrt(C*tau))
           tau=rgamma(1,G+1/2,H+0.5*C*(mu-B)^2)
           output[i,]=c(mu,tau)
    }
    output
}

#' Gibbs sampler for a one-way normal random effects model with a semi-conjugate prior
#' 
#' Simulates realisations from the posterior distribution for the population mean, random effect means 
#' and precision components in a one-way normal random effects model with a semi-conjugate prior
#' @param N length of MCMC chain
#' @param initial starting values for the population mean and the precision components
#' @param priorparam prior parameters a,b,c,d,e,f
#' @param m number of treatments
#' @param n vector containing the number of observations on each treatment
#' @param ybar vector containing the mean of observations on each treatment
#' @param s vector containing the standard deviation of observations on each treatment
#' @export gibbsReffects
#' @examples 
#' data(contamination)
#' n=tapply(contamination$acc,contamination$keyboard,length)
#' ybar=tapply(contamination$acc,contamination$keyboard,mean)
#' s=sqrt(tapply(contamination$acc,contamination$keyboard,var)*(n-1)/n)
#' mcmcAnalysis(gibbsReffects(N=100,initial=c(200,2e-5,1),priorparam=c(200,0.1,0.1,0.1,0.1,0.1),m=10,n=n,ybar=ybar,s=s))
gibbsReffects=function(N,initial,priorparam,m,n,ybar,s)
{
  a=priorparam[1];b=priorparam[2];c=priorparam[3];d=priorparam[4];e=priorparam[5];f=priorparam[6]
  output=matrix(ncol=m+3,nrow=N)
  colnames(output)=c("mu","tau","nu",paste("theta", 1:m, sep=""))
  theta=vector("numeric",length=m)
  mu=initial[1]; tau=initial[2]; nu=initial[3]
    for (j in 1:m) {theta[j]=ybar[j]}
    output[1,]=c(mu,tau,nu,theta)
    for (j in 2:N) {
        muprec=b+m*nu
        mumean=(a*b+m*nu*mean(theta))/muprec
        mu=rnorm(1,mumean,1/sqrt(muprec))
        tauh=d+0.5*sum(n*(s^2+(ybar-theta)^2))
        tau=rgamma(1,c+sum(n)/2,tauh)
        nuh=f+0.5*sum((theta-mu)^2)
        nu=rgamma(1,e+m/2,nuh)
        for (i in 1:m) {
            thetaiprec=nu+n[i]*tau
            thetaimean=(nu*mu+n[i]*ybar[i]*tau)/thetaiprec
            theta[i]=rnorm(1,thetaimean,1/sqrt(thetaiprec))
        }
        output[j,]=c(mu,tau,nu,theta)
    }
    output
}
