#' Elicitation of normal-gamma prior distribution by using quantiles for mu and tau
#' @param mup1 probability.
#' @param mup2 probability.
#' @param taup1 probability.
#' @param taup2 probability.
#' @param muquantile1 lower mup1-th quantile for mu.
#' @param muquantile2 lower mup2-th quantile for mu.
#' @param tauquantile1 lower taup1-th quantile for tau.
#' @param tauquantile2 lower taup2-th quantile for tau.
#' @return Vector containing the parameters of the normal-gamma distribution.
#' @keywords character
#' @importFrom stats optimize qgamma qt
#' @examples
#' b=20;c=10;g=0.15;h=6 # 5% and 95% quantiles are#'
#' muq1=qgt(0.05,2*g,b,h/(g*c));muq2=qgt(0.95,2*g,b,h/(g*c))
#' tauq1=qgamma(0.05,g,h);tauq2=qgamma(0.95,g,h)
#' #The following function determines the original values of b, c, g and h
#' elicitNGa(0.05,muq1,0.95,muq2,0.05,tauq1,0.95,tauq2)
#' @examples
#' elicitNGa(muquantile1=-1,muquantile2=1,tauquantile1=0.1,tauquantile2=1.1)
#' @export
elicitNGa=function(mup1=0.025,muquantile1,mup2=0.975,muquantile2,taup1=0.025,tauquantile1,taup2=0.975,tauquantile2){
  zerofn=function(g){elicitgzerofn(g,taup1,tauquantile1,taup2,tauquantile2)}
  #res=optimize(elicitgzerofn(g,p3,tauquantile3,p4,tauquantile4),interval=c(0,22))
  res=optimize(zerofn,interval=c(0,22))
  g=res$minimum; h=qgamma(taup1,g,1)/tauquantile1
  c=((qt(mup1,2*g)-qt(mup2,2*g))/(muquantile1-muquantile2))^2*(h/g)
  b=muquantile1-qt(mup1,2*g)*sqrt(h/(g*c))
  c(b,c,g,h)
}

#' Function used in the elicitation of normal-gamma prior distribution by using quantiles for mu and tau
#' @param g must be strictly positive.
#' @param taup1 probability.
#' @param taup2 probability.
#' @param tauquantile1 lower taup1-th quantile for tau.
#' @param tauquantile2 lower taup2-th quantile for tau.
#' @return value
#' @keywords character
elicitgzerofn=function(g,taup1,tauquantile1,taup2,tauquantile2)
{
  (qgamma(taup1,g,1)/qgamma(taup2,g,1)-tauquantile1/tauquantile2)^2
}


