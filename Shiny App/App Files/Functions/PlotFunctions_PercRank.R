###################################################
# PERCENT RANK CUSTOM PLOT FUNCTION
# Created by Gottfried Pestal (Solv Consulting Ltd.)
# Version 1 - September 3, 2012
###################################################

# custom function to imitate Excel's percentrank() function
perc.rank<-function(x){
	rank.x<-rank(x, ties.method="min")
	perc.rank.x <- (rank.x-1)/(max(rank.x)-1)
	perc.rank.x
	}

# example
perc.rank(c(1,3,2,4,5,6,7,3,44,5,22,3,3,44,56,90,876,2,0,0,123))


# custom function to plot percent ranks as deviations from median
perc.rank.plot<-function(x,ma=NULL,yrs.lab=c(1950,2010),type="fancy",cex.axis=1.4){
# x is a time series stored in an array, with year labels as dim names
# ma specifies whether to plot a moving average. if ma is a number it defines the period for the avg
	
	if(type=="spark"){barplot(perc.rank(x)-0.5, ylim=c(-0.5,0.5),col="dodgerblue",border="dodgerblue", xlab="", ylab="",axes=FALSE)}


	if(type=="fancy"){
		x.ticks<-barplot(perc.rank(x)-0.5, ylim=c(-0.5,0.5),col="lightblue",border="lightblue", xlab="", ylab="",axes=FALSE)
		abline(h=c(-0.5,0,0.5),col="gray")
		text(rep(-3,3), c(-0.5,0,0.5),adj=1,labels=c("Min","Median","Max"),xpd=NA,cex=1.4)
		if(!is.null(ma)){lines(x.ticks,filter(perc.rank(x)-0.5,filter=rep(1/ma,ma) ,sides=1),col="red",lwd=2)}
		axis(side=1,at=x.ticks[seq(1,length(x.ticks),by=10)],labels = seq(yrs.lab[1],yrs.lab[2],10),cex.axis=cex.axis)
	} # end type=fancy
	
	if(type=="basic"){
		x.ticks <- 1:length(x)
		plot(x.ticks,perc.rank(x)-0.5, ylim=c(-0.5,0.5),col="darkblue",type="l", lwd=2,xlab="", ylab="",axes=FALSE)
		abline(h=c(-0.5,0,0.5),col="gray")
		text(rep(-2.1,3), c(-0.5,0,0.5),adj=1,labels=c("Min","Median","Max"),xpd=NA,cex=1.4)
		if(!is.null(ma)){lines(x.ticks,filter(perc.rank(x)-0.5,filter=rep(1/ma,ma) ,sides=1),col="red",lwd=2)}
		
		axis(side=1,at=x.ticks[seq(1,length(x.ticks),by=10)],labels = seq(yrs.lab[1],yrs.lab[2],10),cex.axis=cex.axis)
	} # end type=basic
		
	
	
	
	
}

