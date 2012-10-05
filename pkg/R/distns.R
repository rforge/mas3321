#' The Inverse Chi distribution
#' 
#' Density, distribution function, quantile function and random generation for the inverse chi distribution with parameters a and b
#' @param x,q vector of quantities
#' @param p vector of probabilities
#' @param n number of observations
#' @param a first parameter. Must be strictly positive.
#' @param b second parameter. Must be strictly positive.
#' @note If X~Inv-Chi(a,b) then it has density f(x)=2b^ax^(-2*a-1)e^(-b/x^2)/Gamma(a). Also 1/X^2~Gamma(a,b).
#' @keywords character
#' @export dinvchi qinvchi pinvchi
#' @examples dinvchi(1, 2, 2)
dinvchi=function(x,a,b){2*dgamma(1./x^2,a,b)/x^3}

#' @rdname dinvchi
#' @examples pinvchi(1, 2, 2)
pinvchi=function(x,a,b){1-pgamma(1./x^2,a,b)}

#' @rdname dinvchi
#' @examples qinvchi(0.95, 2, 2)
qinvchi=function(p,a,b){1/sqrt(qgamma(1-p,a,b))}

#' The Generalised t distribution
#' 
#' Density, distribution function, quantile function and random generation for the generalised t distribution with a degrees of freedom, mean b and scale c
#' @param x,q vector of quantities
#' @param p vector of probabilities
#' @param n number of observations
#' @param a degrees of freedom. Must be strictly positive.
#' @param b mean
#' @param c scale. Must be strictly positive.
#' @note If X~t_a(b,c) then it has density f(x)=(1_(x-b)^2/(ac))^(-(a+1)/2)/(sqrt(ac)*Beta(a,b)). Also (X-b)/sqrt(c)~t_a.
#' @keywords character
#' @export dgt pgt qgt rgt
#' @examples dgt(1, 10, 2, 2)
dgt=function(y,a,b,c){dt((y-b)/sqrt(c),a)/sqrt(c)}

#' @rdname dgt
#' @examples pgt(1, 10, 2, 2)
pgt=function(y,a,b,c){pt((y-b)/sqrt(c),a)}

#' @rdname dgt
#' @examples qgt(0.95, 10, 2, 2)
qgt=function(p,a,b,c){b+sqrt(c)*qt(p,a)}

#' @rdname dgt
#' @examples rgt(1, 10, 2, 2)
rgt=function(n,a,b,c){b+sqrt(c)*rt(n,a)}

#' The Normal-Gamma distribution
#' 
#' Density of the normal-gamma distribution with parameters b, c, g and h.
#' @param mu,tau vector of quantities
#' @param b mean
#' @param c must be strictly positive.
#' @param g must be strictly positive.
#' @param h must be strictly positive.
#' @note If (mu,tau)^T~NGa(b,c,g,h) then mu|tau~N(b,1/(c*tau)) and tau~Ga(g,h). Also mu~t_(2g)(b,h/(gc)).
#' @keywords character
#' @export dnormgamma
#' @examples dnormgamma(1, 2, 1, 2, 5, 3)
dnormgamma=function(mu,tau,b,c,g,h){dnorm(mu,b,1/sqrt(c*tau))*dgamma(tau,g,h)}


#' The Bivariate Normal distribution
#' 
#' Density of the bivariate normal distribution with mean vector b and covariance matrix c.
#' @param x,y vector of quantities
#' @param b mean vector (2x1)
#' @param c covariance matrix. Must be positive definite 2x2 matrix.
#' @note Something here.
#' @keywords character
#' @export dbvnorm
#' @examples dbvnorm(1, 2, c(1,2), diag(1,2))
dbvnorm=function(x,y,b,c){xs=(x-b[1])/sqrt(c[1,1]);ys=(y-b[2])/sqrt(c[2,2]);rho=c[1,2]/sqrt(c[1,1]*c[2,2]);
  dnorm(ys,rho*xs,sqrt(1-rho*rho))*dnorm(xs)/sqrt(c[1,1]*c[2,2])}


#' The Bivariate t distribution
#' 
#' Density of the bivariate t distribution with a degrees of freedom, mean vector b and covariance matrix c.
#' @param x,y vector of quantities
#' @param a degrees of freedom. Must be strictly positive.
#' @param b mean vector (2x1)
#' @param c covariance matrix. Must be positive definite 2x2 matrix.
#' @note Something here.
#' @keywords character
#' @export dbvt
#' @examples dbvt(1, 2, 10, c(1,2), diag(1,2))
dbvt=function(x,y,a,b,c){xs=(x-b[1])/sqrt(c[1,1]);
     ys=(y-b[2])/sqrt(c[2,2]);rho=c[1,2]/sqrt(c[1,1]*c[2,2]);
     dgt(ys,a+1,rho*xs,(1-rho*rho)*(a+xs^2)/(a+1))*dt(xs,a)/sqrt(c[1,1]*c[2,2])}

