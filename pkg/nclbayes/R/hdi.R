#' The highest density interval (HDI) for a Beta(a,b) distribution
#' @param p confidence level for the HDI.
#' @param a a positive number.
#' @param b a positive number.
#' @return Vector containing the limits of the HDI.
#' @keywords character
#' @importFrom stats qbeta dbeta pbeta pgamma
#' @examples
#' hdiBeta(0.9, 2, 2)
#' @export
hdiBeta=function(p=0.95,a,b)
{
  if (a==b) {
    lower=qbeta((1-p)/2,a,b); upper=qbeta((1+p)/2,a,b)
  } else if ((a<=1)&(b>1)) {
    lower=0; upper=qbeta(p,a,b)
  } else if ((a>1)&(b<=1)) {
    lower=qbeta(1-p,a,b); upper=1
  } else {
    zerofn=function(x){(dbeta(qbeta(p+pbeta(x,a,b),a,b),a,b)-dbeta(x,a,b))^2}
    maxl=qbeta(1-p,a,b)
    res=optimize(zerofn,interval=c(0,maxl))
    lower=res$minimum; upper=qbeta(p+pbeta(lower,a,b),a,b)
  }
  c(lower,upper)
}

#' The highest density interval (HDI) for a Gamma(a,b) distribution
#' @inheritParams hdiBeta
#' @return Vector containing the limits of the HDI.
#' @keywords character
#' @examples
#' hdiGamma(0.9, 2, 2)
#' @export
hdiGamma=function(p=0.95,a,b)
{
  if (a<=1) {
    lower=0; upper=qgamma(p,a,b)
  } else {
    zerofn=function(x){(dgamma(qgamma(p+pgamma(x,a,b),a,b),a,b)-dgamma(x,a,b))^2}
    maxl=qgamma(1-p,a,b)
    res=optimize(zerofn,interval=c(0,maxl))
    lower=res$minimum; upper=qgamma(p+pgamma(lower,a,b),a,b)
  }
  c(lower,upper)
}



#' The highest density interval (HDI) for a Inv-Chi(a,b) distribution
#' @inheritParams hdiBeta
#' @return Vector containing the limits of the HDI.
#' @keywords character
#' @examples
#' hdiInvchi(0.9, 2, 2)
#' @export
hdiInvchi=function(p=0.95,a,b)
{
   zerofn=function(x){(dinvchi(qinvchi(p+pinvchi(x,a,b),a,b),a,b)-dinvchi(x,a,b))^2}
   maxl=qinvchi(1-p,a,b)
   res=optimize(zerofn,interval=c(0,maxl))
   lower=res$minimum; upper=qinvchi(p+pinvchi(lower,a,b),a,b)
   c(lower,upper)
}
