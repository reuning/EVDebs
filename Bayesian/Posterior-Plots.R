post.plot <- function(posterior, param.to.plot=colnames(posterior), 
                      param.n=param.to.plot, order.t = F,
                      xmax=max(posterior[,param.to.plot]),
                      xmin=min(posterior[,param.to.plot]), 
                      ymin=1, ymax=length(param.to.plot)+.1, ab=0, 
                      y.lab=T, p.dens=T, id.lines=T, ...){
  #   Posterior is the matrix of the posterior (use as.matrix() if it is an mcmc object)
  #   Param.to.plot is used to select the parameters to plot, defaults to all. 
  #   param.n is the names used 
  #   xmax, xmin, ymin and max are used for plotting, min and max of axes
  #   ab - do you want an ab line at 0
  #   y.lab - do you want the y axis to have labels
  #   p.dens - do you want to plot the density of the posteriors
  
  par(...)
  plot.new()
  plot.window(xlim=c(xmin, xmax),ylim=c(ymin, ymax))
  conv <- 1/(1.2*max(apply(posterior[,param.to.plot], 2, function(ii) max(density(ii)$y))))
  if(order.t){
    od <- order(apply(posterior[,param.to.plot], 2, mean))
  } else { od <- length(param.to.plot):1 }
  for(ii in length(param.to.plot):1){
    tmp <- posterior[,param.to.plot[od[ii]]]
    points(y=ii, x=mean(tmp))
    ci <- quantile(tmp, c(0.025, .975))
    lines(y=c(ii,ii), x=ci, lwd=2)
    if(id.lines){
      lines(y=c(ii,ii), x=c(xmin,xmax), lty=3, lwd=.5)
    }
    if(p.dens){
      den <- density(tmp)
      lines(y=den$y*conv+ii, x=den$x, lty=2) 
    }
    
    
  }
  xlimlo <- floor(xmin)
  xlimup <- ceiling(xmax)
  n.ax <- length(xlimlo:xlimup)
  if(n.ax < 5){
    x.ax <- xlimlo:xlimup
  } else {
    n.by <- n.ax %/% 5 
    x.ax <- seq(xlimlo, xlimup, by = n.by)
    
  }
  axis(1, at=x.ax)
  if(y.lab){
    axis(2, at=1:length(param.to.plot), param.n, las=1)
  } else {
    axis(2, at=1:length(param.to.plot), label=NA, tck=0)
  }
  if(ab | ab==0){
    abline(v=ab)
  }
}
