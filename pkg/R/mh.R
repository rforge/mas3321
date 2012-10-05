metrop=function(N,initial,a,show=TRUE) 
{
    output=vector("numeric",N)
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
        output[i]=x
    }
    if (show==TRUE) message(paste("Acceptance rate = ",naccept/N))
    output
}


mh=function(N,initial,intau,innu,a,b,c,d,e,f,m,n,ybar,s,show=TRUE) 
{
    output=matrix(ncol=3,nrow=N)
    colnames(output)=c("mu","tau","nu")
    mu=initial[1]; tau=initial[2]; nu=initial[3]
    output[1,]=c(mu,tau,nu)
    naccepttau=0; nacceptnu=0
    for (i in 2:N) {
        muprec=b+tau*nu*sum(n/(nu+n*tau))
        mumean=(a*b+tau*nu*sum(n*ybar/(nu+n*tau)))/muprec
        mu=rnorm(1,mumean,1/sqrt(muprec))
        proptau=rnorm(1,tau,intau)
        if (proptau>0) {
           alprob=lfcdtau(proptau,mu,nu,c,d,m,n,ybar,s)-
                      lfcdtau(tau,mu,nu,c,d,m,n,ybar,s)
           lu=log(runif(1))
           if (lu<alprob) {
              tau=proptau
              naccepttau=naccepttau+1
           }
        }
        propnu=rnorm(1,nu,innu)
        if (propnu>0) {
           alprob=lfcdnu(propnu,mu,tau,e,f,m,n,ybar)-
                      lfcdnu(nu,mu,tau,e,f,m,n,ybar)
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

lfcdtau=function(tau,mu,nu,c,d,m,n,ybar,s)
{(c+sum(n)/2-1)*log(tau)-d*tau-0.5*sum(log(nu+n*tau))-
          0.5*tau*sum(n*s^2)-0.5*tau*nu*sum(n*(ybar-mu)^2/(nu+n*tau))}

lfcdnu=function(nu,mu,tau,e,f,m,n,ybar)
{(e+m/2-1)*log(nu)-f*nu-0.5*sum(log(nu+n*tau))-
             0.5*tau*nu*sum(n*(ybar-mu)^2/(nu+n*tau))}



mh2=function(N,initial,intau,innu,a,b,c,d,e,f,m,n,ybar,s,show=TRUE) 
{
    output=matrix(ncol=3,nrow=N)
    colnames(output)=c("mu","tau","nu")
    mu=initial[1]; tau=initial[2]; nu=initial[3]
    output[1,]=c(mu,tau,nu)
    naccepttau=0; nacceptnu=0
    for (i in 2:N) {
        muprec=b+tau*nu*sum(n/(nu+n*tau))
        mumean=(a*b+tau*nu*sum(n*ybar/(nu+n*tau)))/muprec
        mu=rnorm(1,mumean,1/sqrt(muprec))
        proptau=exp(rnorm(1,log(tau),intau))
        alprob=lfcdtau(proptau,mu,nu,c,d,m,n,ybar,s)-
                lfcdtau(tau,mu,nu,c,d,m,n,ybar,s)+log(proptau)-log(tau)
        lu=log(runif(1))
        if (lu<alprob) {
           tau=proptau
           naccepttau=naccepttau+1
        }
        propnu=exp(rnorm(1,log(nu),innu))
        alprob=lfcdnu(propnu,mu,tau,e,f,m,n,ybar)-
                   lfcdnu(nu,mu,tau,e,f,m,n,ybar)+log(propnu)-log(nu)
        lu=log(runif(1))
        if (lu<alprob) {
           nu=propnu
           nacceptnu=nacceptnu+1
        }
        output[i,]=c(mu,tau,nu)
    }
    if (show==TRUE) {
       message(paste("Acceptance rate for tau = ",naccepttau/N))
       message(paste("Acceptance rate for nu = ",nacceptnu/N))
    }
    output
}
