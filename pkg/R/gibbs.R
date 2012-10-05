gibbsnorm=function(N,initial,b,c,g,h,n,xbar,s)
{
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


gibbsnorm2=function(N,initial,b,c,g,h,n,xbar,s)
{
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


gibbsnorm3=function(N,initial,a,b,c,d,e,f,m,n,ybar,s)
{
    output=matrix(ncol=m+3,nrow=N)
    colnames(output)=c("mu","tau","nu",paste("theta", 1:m, sep=""))
    theta=matrix(ncol=m,nrow=1)
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
