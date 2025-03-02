################################################################################
if(1) {cat("### load monbart code\n")
source("../../code/load-monbart.R")
}
################################################################################
if(1) {cat("### simulate data\n")
set.seed(99)
n=200
x = matrix(sort(-1+2*runif(n)),ncol=1)
y = x^3 + .1*rnorm(n)
}
################################################################################
if(1) {cat("### run parallel rpmonbart\n")
set.seed(99)
bfmc = rpmonbart(x,y,x,mc.cores=8)
}
################################################################################
if(1) {cat("### plot fit\n")
qvec=c(.025,.975) #quantiles for point wise intervals

par(mfrow=c(1,1))
plot(x,y,type="n") #create plot
qmc = apply(bfmc$yhat.train,2,quantile,probs=qvec) #get posterior intervals
polygon(c(x,rev(x)),c(qmc[1,],rev(qmc[2,])),col="grey",border=FALSE) #add intervals to plot
lines(x,bfmc$yhat.train.mean,col="blue",lwd=2) #add fit to plot
lines(x,x^3,col="red",lwd=2) #add true f to plot
points(x,y,pch=".",col="black",cex=3) #add data to plot
title(main="95% pointwise posterior intervals, mbart")
}
