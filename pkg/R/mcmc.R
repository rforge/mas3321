mcmcProcess=function(input,burnin=1000,thin=1)
{
    N=nrow(input); p=ncol(input)
    Nout=floor((N-burnin)/thin)
    output=matrix(ncol=p,nrow=Nout)
    colnames(output)=colnames(input)
    for (i in 1:Nout){output[i,]=input[burnin+1+thin*(i-1),]}
    output
}

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

mcmcBoxplot=function(input,start=1,end=NULL)
{
    if (is.null(end)) end=ncol(input)
    labels=colnames(input)[start:end]
    N=nrow(input)
    dd=data.frame(value=as.vector(input[,start:end]),variables=rep(labels,N))
    boxplot(value~variables,dd)
}
