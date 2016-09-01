#' Calculates a quadratic form for 2-d vectors
#' @param beta1 dimension 1.
#' @param beta2 dimension 2.
#' @param b 2-d vector containing (location) mean.
#' @param c 2x2 centering matrix.
#' @examples
#' quadform2d(1,1,c(0,0),diag(1,2))
#' @export
quadform2d=function(beta1,beta2,b,c){
  c[1,1]*(beta1-b[1])^2+c[2,2]*(beta2-b[2])^2+2*c[1,2]*(beta1-b[1])*(beta2-b[2])
}

#' Parameters of the NGa posterior distribution when sampling from a normal population
#' and using a NGa prior distribution
#' @param priorpara vector of prior parameters b,c,g,h.
#' @param n sample size.
#' @param xbar sample mean.
#' @param s sample standard deviation.
#' @examples
#' NGaposterior(c(0,1,1,1),10,1,1)
#' @export
NGaposterior=function(priorpara,n,xbar,s){
  b=priorpara[1];c=priorpara[2];g=priorpara[3];h=priorpara[4];
  c((b*c+n*xbar)/(c+n),c+n,g+n/2,h+c*n*(xbar-b)^2/(2*(c+n))+n*s^2/2)
}
