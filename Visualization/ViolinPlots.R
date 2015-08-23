vplot <- function(y=NULL,x=NULL, data=NULL){
  if(is.null(y)) break("Must provide a y value")
  if(!is.null(data)){
    arguments <- as.list(match.call())
    x <- eval(arguments$x, data)
    y <- eval(arguments$y, data)
  }
  
  if(is.null(x)){
    dens <- density(y, na.rm=T)
    
    plot.new()
    plot.window(xlim=c(-max(dens$y)*1.2, max(dens$y)*1.2 ),ylim=c(min(dens$x), max(dens$x)))
    lines(x=dens$y, y=dens$x)
    lines(x=-dens$y, y=dens$x)
    axis(1, at=c(-max(dens$y)*1.2, max(dens$y)*1.2 ), labels=c("",""), lwd.ticks=0, line=1)
    axis(2, lwd.ticks=0, line=1)
    box(lwd=.1, col="gray")
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