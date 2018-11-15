###################################################
# COMPARE 2 TIME SERIES CUSTOM PLOT FUNCTION
# Created by Gottfried Pestal (Solv Consulting Ltd.)
# Version 1 - October 25, 2016
###################################################


# custom function to plot time series
ts.comp.plot<-function(x,y1,y2,xlim=c(1950,2010),grid=pretty(c(y1,y2),n=4),grid.label=TRUE, minmaxpts=TRUE,
						y1.label="Y1",y2.label="Y2"){
# ma specifies whether to plot a moving average. if ma is a number it defines the period for the avg
	# set the switch between writing full numbers and using scientific notation in labels
	options(scipen=3)
	# plot the time seris
	plot(x,y1, xlim=xlim,ylim=c(min(grid),max(grid)),type="l",col="blue",axes=FALSE,xlab="",ylab="",lwd=1,xpd=TRUE)
	# add gridlines
	segments(x0=rep(xlim[1],length(grid)) , y0=grid, x=rep(xlim[2],length(grid)) , y1=grid  ,col="grey",lty=2,xpd=NA)
	if(grid.label){text(x=rep(xlim[1]-1,length(grid)),y=grid,labels=prettyNum(grid,big.mark=","),cex=1.4, adj=1,xpd=NA)}
	# add year axis
	xlab.vals <- xlim[1]:xlim[2]
	xlab.labels <- pretty(xlab.vals ,n=length(xlab.vals)/4)
	xlab.labels <- xlab.labels[ xlab.labels %in% xlab.vals]
	axis(side=1,at=xlab.labels ,labels = xlab.labels ,cex.axis=1.4)

	lines(x,y2, type="l",col="red",lty=1,lwd=1,xpd=NA)
	lines(x,y1, type="l",col="blue",lty=1,lwd=2,xpd=NA)


	if(minmaxpts==TRUE){
		# calc, plot, and label max point
		points(x[y1==max(y1,na.rm=TRUE)],y1[y1==max(y1,na.rm=TRUE)],col="darkblue",pch=21,bg="green",lwd=1.2,cex=1.5)
		text(x[y1==max(y1,na.rm=TRUE)],y1[y1==max(y1,na.rm=TRUE)] ,labels=paste(prettyNum(round(y1[y1==max(y1,na.rm=TRUE)],2),big.mark=",")," (",x[y1==max(y1,na.rm=TRUE)],")",sep=""),pos=2,cex=1.1,xpd=NA)

		# calc, plot, and label min point
		points(x[y1==min(y1,na.rm=TRUE)],y1[y1==min(y1,na.rm=TRUE)],col="darkblue",pch=21,bg="red",lwd=1.2,cex=1.5)
		text(x[y1==min(y1,na.rm=TRUE)],y1[y1==min(y1,na.rm=TRUE)] ,labels=paste(prettyNum(round(y1[y1==min(y1,na.rm=TRUE)],2),big.mark=",")," (",x[y1==min(y1,na.rm=TRUE)],")",sep=""),pos=2,cex=1.1,xpd=NA)
		} # end if minmax pts =TRUE
	legend(x=xlim[1]+((xlim[2]-xlim[1])*0.8),y=max(grid)*1.25,legend=c(y1.label,y2.label),col=c("blue","red"),lty=c(1,1),lwd=c(2,1),bty="n"  ,xpd=NA)
		#legend("topright",legend=c(y1.label,y2.label),col=c("blue","tomato"),lty=c(1,2),bty="n",  ,xpd=NA)
		
		
		
} # end ts.comp.plot function


