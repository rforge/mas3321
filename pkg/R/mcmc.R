#' Function to remove burn-in and thin MCMC output
#' 
#' Function chops off burnin iterations and then thins the output by taking every thin iterates
#' @param input matrix containing MCMC realisations
#' @param burnin number of initial iterations to delete
#' @param thin level of thinning
#' @export mcmcProcess
mcmcProcess=function(input,burnin=1000,thin=1)
{
  output=input[seq(burnin + 1, nrow(input), by=thin),]
  if (ncol(input)==1){
    as.matrix(output)}
  else {output}
  }


#' Equi-tailed confidence intervals from MCMC output
#' @param input matrix containing MCMC realisations
#' @param level confidence level required
#' @export mcmcCi
mcmcCi=function(input,level=0.95)
{
    N=nrow(input); p=ncol(input)
    output=matrix(ncol=2,nrow=p)
    rownames(output)=colnames(input)
    for (i in 1:p) {
        x=sort(input[,i])
        output[i,1]=x[floor(N*(1-level)/2)]
        output[i,2]=x[floor(N*(1+level)/2)]
    }
    output
}

#' Box plots from MCMC output
#' @param input matrix containing MCMC realisations
#' @param start take realisations starting at start
#' @param end and ending at end
#' @export mcmcBoxplot
mcmcBoxplot=function(input,start=1,end=NULL)
{
    if (is.null(end)) end=ncol(input)
    labels=colnames(input)[start:end]
    N=nrow(input)
    dd=data.frame(value=as.vector(input[,start:end]),variables=rep(labels,N))
    boxplot(value~variables,dd)
}
