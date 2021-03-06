#' Summarise and plot tabular MCMC output
#'
#' This function summarises and plots tabular MCMC output such as that generated by the function normgibbs.
#' @param mat matrix of MCMC output, where the columns represent variables and the rows represent iterations.
#' @param rows number of variables to plot per page on the graphics device.
#' @param lag.max maximum lag for the ACF plots.
#' @param bins approximate number of bins to use for the histograms.
#' @param show a logical. If TRUE, will display numerical summaries on the R console.
#' @param plot a logical. If TRUE, will plot graphical summaries on the default graphics device.
#' @references A version of mcmcSummary in Darren Wilkinson's package smfsb.
#' @importFrom stats sd acf ts
#' @importFrom graphics par hist boxplot
#' @examples
#' posterior=gibbsNormal(N=1000,initial=c(10,0.25),priorparam=c(10,1/100,3,12),n=100,xbar=15,s=4.5)
#' mcmcAnalysis(posterior,rows=2,bins=10)
#' @export mcmcAnalysis
mcmcAnalysis=function (mat, rows = 4, lag.max = 100, bins = 30, show = TRUE, plot = TRUE)
{
  d = dim(mat)
  p = d[2]
  summ = summary(mat)
  if (show == TRUE) {
    message(paste("N =", d[1], "iterations"))
    print(summ)
    message("Standard deviations:")
    print(apply(mat, 2, sd))
  }
  if (plot == TRUE) {
    names = colnames(mat)
    op = par(mfrow = c(rows, 3))
    for (i in 1:p) {
      plot(ts(mat[, i]), main = names[i], ylab = "Value",
           xlab = "Iteration")
      acf(mat[, i], lag.max = lag.max, main = names[i],
          ci = 0)
      hist(mat[, i], bins, main = names[i], xlab = "Value",
           freq = FALSE)
    }
    par(op)
  }
  invisible(summ)
}

#' Function to remove burn-in and thin MCMC output
#'
#' Function chops off burnin iterations and then thins the output by taking every thin iterates.
#' @param input matrix containing MCMC realisations.
#' @param burnin number of initial iterations to delete.
#' @param thin level of thinning.
#' @export
mcmcProcess=function(input,burnin=1000,thin=1)
{
  output=input[seq(burnin + 1, nrow(input), by=thin),]
  if (ncol(input)==1){
    as.matrix(output)
  }
  else {output}
}


#' Equi-tailed confidence intervals from MCMC output
#' @param input matrix containing MCMC realisations.
#' @param level confidence level required.
#' @export
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
#' @param input matrix containing MCMC realisations.
#' @param start take realisations starting at start.
#' @param end and ending at end.
#' @export
mcmcBoxplot=function(input,start=1,end=NULL)
{
    if (is.null(end)) end=ncol(input)
    labels=colnames(input)[start:end]
    N=nrow(input)
    dd=data.frame(value=as.vector(input[,start:end]),variables=rep(labels,N))
    boxplot(value~variables,dd)
}
