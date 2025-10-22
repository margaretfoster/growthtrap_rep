plotContinuousAlt <- function(prep,covariate,topics, cdata, cmat, simbetas, offset,xlab=NULL,
                           ylab=NULL, main=NULL, xlim=NULL, ylim=NULL,
                           linecol=NULL, add=F, labeltype,n=7, custom.labels=NULL,model=NULL,frexw=.5,printlegend=T,
                           omit.plot=FALSE,
                           ...){
  #What are the unique values of the covariate we are going to plot over?
  uvals <- cdata[,covariate]
  
  ## Patch for stm v 1.3 +, which changed object formats
  if (is.null(prep$topics) && !is.null(prep$parameters$topics)) {
    prep$topics <- prep$parameters$topics
  }
  
  #For each topic, 1. Simulate values, 2. Find means and cis
  means = list()
  cis = list()
  for(i in 1:length(topics)){
    #Simulate values
    sims <- cmat%*%t(simbetas[[which(prep$topics==topics[i])]])
    #Find means and cis
    means[[i]] <- rowMeans(sims)
    cis[[i]] = apply(sims,1, function(x) quantile(x, c(offset,1-offset)))
  }

  #Get the plot set up
  if(!omit.plot) {
    if (is.null(xlim)) xlim <- c(min(uvals), max(uvals))
    if (is.null(ylim)) ylim <- c(min(unlist(cis), na.rm=T),
                                 max(unlist(cis), na.rm=T))
    if (is.null(ylab)) ylab <- "Expected Topic Proportion"
    if (add==F) plot(0, 0,col="white",xlim=xlim, ylim=ylim, main=main,
                             xlab=xlab, ylab=ylab,  ...)
    if (is.null(linecol)) cols = grDevices::rainbow(length(topics))
    if (!is.null(linecol)) cols=linecol
  
    #Plot everything
    for(i in 1:length(topics)){
      lines(uvals, means[[i]], col=cols[i])
      lines(uvals, cis[[i]][1,], col=cols[i], lty=2)
      lines(uvals, cis[[i]][2,], col=cols[i], lty=2)
    }
  }
  #Create legend
  if(printlegend==T){
    labels = createLabels(labeltype=labeltype, covariate=covariate, method="continuous",
      cdata=cdata, cov.value1=NULL, cov.value2=NULL,model=model,n=n,
      topics=topics,custom.labels=custom.labels, frexw=frexw)
    if(!omit.plot) legend(xlim[1], ylim[2], labels, cols)
    return(invisible(list(x=uvals, topics=topics,means=means, ci=cis, labels=labels)))
  }else{
    return(invisible(list(x=uvals, topics=topics,means=means, ci=cis)))
  }
  
}
