vplot <- function(y=NULL,x=NULL, data=NULL){
  if(!is.null(data)){
    arguments <- as.list(match.call())
    x <- eval(arguments$x, data)
    y <- eval(arguments$y, data)
  }
  
  if(is.null(x)){
    plot.new()
    plot.window(xli=c(0, 2),ylim=c(min(y, na.rm=T), max(y, na.rm=T)))
    dens <- density(y, na.rm=T)
    lines(x=1+dens$y, y=dens$x)
    ines(x=1-dens$y, y=dens$x)
    axis(1)
    axis(2)
  } else {
    fact <- as.factor(x)
    numb <- length(levels(fact))
    y.max=ceiling(max(sapply(levels(fact), function(ii) max(density(y[fact==ii], na.rm=T)$x))))
    y.min=floor(min(sapply(levels(fact), function(ii) min(density(y[fact==ii], na.rm=T)$x))))
    
    max.dens <- max(sapply(levels(fact), function(ii) max(density(y[fact==ii], na.rm=T)$y)))*1.2
    dist <- max.dens*2
    x.max <- dist*numb
    
    plot.new()
    plot.window(xlim=c(1*dist/1.2-max.dens, x.max),ylim=c(y.min, y.max))
    for(ii in 1:numb){
      dens <- density(y[fact==levels(fact)[ii]], na.rm=T)
      lines(x=(ii*dist/1.2)+dens$y, y=dens$x)
      lines(x=(ii*dist/1.2)-dens$y, y=dens$x)
    }
    axis(1, at=c(1*dist/1.2-max.dens,(1:numb)*dist/1.2,x.max), labels=c("",levels(fact),""), lwd.ticks=0, line=1)
    axis(2, line=1)
    box(lwd=.1, col="gray")
  }
  
}