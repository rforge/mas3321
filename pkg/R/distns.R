#' The Inverse Chi distribution
#' 
#' Density, distribution function, quantile function and random generation for the inverse chi distribution with parameters a and b
#' @param x vector of quantities
#' @param p vector of probabilities
#' @param n number of observations
#' @param a first parameter. Must be strictly positive.
#' @param b second parameter. Must be strictly positive.
#' @note If X~Inv-Chi(a,b) then it has density f(x)=2b^ax^(-2*a-1)e^(-b/x^2)/Gamma(a). Also 1/X^2~Gamma(a,b).
#' @keywords character
#' @export dinvchi qinvchi pinvchi rinvchi
#' @examples dinvchi(1, 2, 2)
dinvchi=function(x,a,b){2*dgamma(1./x^2,a,b)/x^3}

#' @rdname dinvchi
#' @examples pinvchi(1, 2, 2)
pinvchi=function(x,a,b){1-pgamma(1./x^2,a,b)}

#' @rdname dinvchi
#' @examples qinvchi(0.95, 2, 2)
qinvchi=function(p,a,b){1/sqrt(qgamma(1-p,a,b))}

#' @rdname dinvchi
#' @examples rinvchi(1, 2, 2)
rinvchi=function(n,a,b){1/sqrt(rgamma(n,a,b))}

#' The Generalised t distribution
#' 
#' Density, distribution function, quantile function and random generation for the generalised t distribution with a degrees of freedom, mean b and scale c
#' @param x vector of quantities
#' @param p vector of probabilities
#' @param n number of observations
#' @param a degrees of freedom. Must be strictly positive.
#' @param b mean
#' @param c scale. Must be strictly positive.
#' @note If X~t_a(b,c) then it has density f(x)=(1_(x-b)^2/(ac))^(-(a+1)/2)/(sqrt(ac)*Beta(a,b)). Also (X-b)/sqrt(c)~t_a.
#' @keywords character
#' @export dgt pgt qgt rgt
#' @examples dgt(1, 10, 2, 2)
dgt=function(x,a,b,c){dt((x-b)/sqrt(c),a)/sqrt(c)}

#' @rdname dgt
#' @examples pgt(1, 10, 2, 2)
pgt=function(x,a,b,c){pt((x-b)/sqrt(c),a)}

#' @rdname dgt
#' @examples qgt(0.95, 10, 2, 2)
qgt=function(p,a,b,c){b+sqrt(c)*qt(p,a)}

#' @rdname dgt
#' @examples rgt(1, 10, 2, 2)
rgt=function(n,a,b,c){b+sqrt(c)*rt(n,a)}

#' The Normal-Gamma distribution
#' 
#' Density and random generation for the normal-gamma distribution with parameters b, c, g and h.
#' Also contours and confidence regions.
#' @param mu vector of quantities
#' @param tau vector of quantities
#' @param b mean
#' @param c must be strictly positive.
#' @param g must be strictly positive.
#' @param h must be strictly positive.
#' @param n sample size
#' @note If (mu,tau)^T~NGa(b,c,g,h) then mu|tau~N(b,1/(c*tau)) and tau~Ga(g,h). Also mu~t_(2g)(b,h/(gc)).
#' @keywords character
#' @export dnormgamma rnormgamma
#' @examples dnormgamma(1, 2, 1, 2, 5, 3)
dnormgamma=function(mu,tau,b,c,g,h){dnorm(mu,b,1/sqrt(c*tau))*dgamma(tau,g,h)}

#' @rdname dnormgamma
#' @examples rnormgamma(1, 1, 2, 5, 3)
rnormgamma=function(n,b,c,g,h){output=matrix(ncol=2,nrow=n);colnames(output)=c("mu","tau");output[,2]=rgamma(n,g,h);output[,1]=rnorm(n,b,1/sqrt(c*output[,2]));output}

#' @rdname dnormgamma
#' @param p probability
#' @param ... Arguments to be passed to the plot function when plotting contours
#' @export NGacontour
#' @examples 
#' mu=seq(2,4,len=1000)
#' tau=seq(0,30,len=1000)
#' NGacontour(mu,tau,3,1,4,0.35,0.95)
#' NGacontour(mu,tau,2.5,30,20,2,0.95,add=TRUE,lty=3)
NGacontour <- function (mu, tau, b, c, g, h, p=NULL,...) {
  pdf=function(mu,tau){dnormgamma(mu,tau,b,c,g,h)}
  z=outer(mu,tau,pdf)
  if (length(p)==0) {
    contour(mu,tau,z,xlab=expression(mu),ylab=expression(tau),...)}
  else {
    NGaclevels=function(p,b,c,g,h){n=1000
                                   tau=rgamma(n,g,h)
                                   mu=rnorm(n,b,1/sqrt(c*tau))
                                   y=dnormgamma(mu,tau,b,c,g,h)
                                   y=sort(y)
                                   y[floor(n*(1-p))]}  
    pdflevels=NGaclevels(p,b,c,g,h)
    contour(mu,tau,z,levels=pdflevels,xlab=expression(mu),ylab=expression(tau),...)}
}

#' The Normal-InvChi distribution
#' 
#' Density and random generation for the normal-inv-chi distribution with parameters b, c, g and h.
#' @param mu vector of quantities
#' @param sigma vector of quantities
#' @param b mean
#' @param c must be strictly positive.
#' @param g must be strictly positive.
#' @param h must be strictly positive.
#' @param n number of observations
#' @note If (mu,sigma)^T~NInvChi(b,c,g,h) then mu|sigma~N(b,sigma^2/c) and sigma~Inv-Chi(g,h). Also mu~t_(2g)(b,h/(gc)).
#' @keywords character
#' @export dnorminvchi rnorminvchi
#' @examples dnorminvchi(1, 1/sqrt(2), 1, 2, 5, 3)
dnorminvchi=function(mu,sigma,b,c,g,h){dnorm(mu,b,sigma/sqrt(c))*dinvchi(sigma,g,h)}

#' @rdname dnorminvchi
#' @examples rnorminvchi(1, 1, 2, 5, 3)
rnorminvchi=function(n,b,c,g,h){output=matrix(ncol=2,nrow=n);colnames(output)=c("mu","sigma");output[,2]=rinvchi(n,g,h);output[,1]=rnorm(n,b,output[,2]/sqrt(c));output}

#' The Bivariate Normal distribution
#' 
#' Density of the bivariate normal distribution with mean vector b and covariance matrix c.
#' @param x vector of quantities
#' @param y vector of quantities
#' @param b mean vector (2x1)
#' @param c covariance matrix. Must be positive definite 2x2 matrix.
#' @note Something here.
#' @keywords character
#' @export dbvnorm
#' @examples dbvnorm(1, 2, c(1,2), diag(1,2))
dbvnorm=function(x,y,b=c(0,0),c=diag(1,2)){
    xs=(x-b[1])/sqrt(c[1,1])
    ys=(y-b[2])/sqrt(c[2,2])
    rho=c[1,2]/sqrt(c[1,1]*c[2,2])
    dnorm(ys,rho*xs,sqrt(1-rho*rho))*dnorm(xs)/sqrt(c[1,1]*c[2,2])}


#' The Bivariate t distribution
#' 
#' Density (and its contours) of the bivariate t distribution with a degrees of freedom, mean vector b and covariance matrix c.
#' @param x vector of quantities
#' @param y vector of quantities
#' @param a degrees of freedom. Must be strictly positive.
#' @param b mean vector (2x1)
#' @param c covariance matrix. Must be a positive definite 2x2 matrix.
#' @keywords character
#' @export dbvt
#' @examples dbvt(1, 2, 10, c(1,2), diag(1,2))
dbvt=function(x,y,a,b,c){xs=(x-b[1])/sqrt(c[1,1])
     ys=(y-b[2])/sqrt(c[2,2])
     rho=c[1,2]/sqrt(c[1,1]*c[2,2])
     dgt(ys,a+1,rho*xs,(1-rho*rho)*(a+xs^2)/(a+1))*dt(xs,a)/sqrt(c[1,1]*c[2,2])}

#' @rdname dbvt
#' @param p probability
#' @param ... Arguments to be passed to the plot function when plotting contours
#' @export tcontour
#' @examples 
#' beta1=seq(-3,3,len=100)
#' beta2=seq(-3,3,len=100)
#' tcontour(beta1,beta2,10,c(0,0),matrix(c(1,0.8,0.8,1),ncol=2))
tcontour=function (x,y,a,b,c,p=NULL, ...) {
  pdf=function(x,y){dbvt(x,y,a,b,c)}
  z=outer(x,y,pdf)
  if (length(p)==0) {
    contour(x,y,z,...)}
  else {
    pdflevels=(gamma(a/2+1)/(sqrt(det(c))*a*pi*gamma(a/2)))*(1+2*qf(p,2,a)/a)^(-a/2-1)
    contour(x,y,z,levels=pdflevels,...)
  }
}

    