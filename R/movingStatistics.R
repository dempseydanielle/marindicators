#'@importFrom stats median
#'@importFrom stats sd
#'@importFrom stats var

movingStatistics<- function(x, n=1,stat=c('mean','median','cv','sd','var')) {

#calculates the running statistics for a time series of data

    before <- floor  ((n-1)/2)
    after  <- ceiling((n-1)/2)

    s     <- rep(0, length(x))
    count <- rep(0, length(x))
    new <- x
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    i <- 1
    while (i <= before) {
        new   <- c(rep(NA, i), x[1:(length(x)-i)])
        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new
        i <- i+1
       }
    i <- 1
    while (i <= after) {
        new   <- c(x[(i+1):length(x)], rep(NA, i))
        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new
        i <- i+1
    }

    o <- length(count)
    u <- ceiling(n/2)
    ind.con <- matrix(NA,nrow=o,ncol=o)
    	for( i in 1:length(count)) {
			if(i<u) {
    		b <- c(rep(1,count[i]))
    		p <- length(b)
    		ind<-c(b,rep(NA,o-p))
    		}
    		if(i>=u & i<o) {
    		  n0 <- rep(NA,i-u)
    		  b <- c(n0,rep(1,count[i]))
    		  p <- length(b)
    		  ind<-c(b,rep(NA,o-p))
    		}
    		if(i==o) {
    		  n0 <- rep(NA,i-u)
    		  b <- c(n0,rep(1,count[i]))
    		  p <- length(b)
    		  ind<-c(b)
    		}
    		ind.con[i,]<-ind
    	}
   out <- numeric(o) 	
  if(stat=='mean') {
  	for(i in 1:nrow(ind.con)) {
  		out[i] <- mean(x*ind.con[i,],na.rm=T)
  	}
  }
  if(stat=='median') {
  for(i in 1:nrow(ind.con)) {
  		out[i] <- median(x*ind.con[i,],na.rm=T)
  	}
  }
  if(stat=='cv') {
  for(i in 1:nrow(ind.con)) {
  		out[i] <- sd(x*ind.con[i,],na.rm=T)/mean(x*ind.con[i,],na.rm=T)
  	}
  }
  if(stat=='sd') {
  for(i in 1:nrow(ind.con)) {
  		out[i]<-sd(x*ind.con[i,],na.rm=T)
  }
  }
  if(stat=='var') {
  for(i in 1:nrow(ind.con)) {
  			out[i]<-var(x*ind.con[i,],na.rm=T)
  }  
  }
	return(out)  	   
}
	