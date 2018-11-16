###################################################
# FANCY TIME SERIES CUSTOM PLOT FUNCTION
# Created by Gottfried Pestal (Solv Consulting Ltd.)
# Version 1 - September 3, 2012
###################################################


# custom function to plot time series
ts.plot<-function(x,y,xlim=c(1950,2010),grid=pretty(y,n=4),grid.label=TRUE, ma=NULL, minmaxpts=TRUE,cex.axis=1.4){
# ma specifies whether to plot a moving average. if ma is a number it defines the period for the avg
	# set the switch between writing full numbers and using scientific notation in labels
	options(scipen=3)
	# plot the time seris
	plot(x,y, xlim=xlim,ylim=c(min(grid),max(grid)),type="l",col="blue",axes=FALSE,xlab="",ylab="",lwd=2,xpd=TRUE)
	# add gridlines
	segments(x0=rep(xlim[1],length(grid)) , y0=grid, x=rep(xlim[2],length(grid)) , y1=grid  ,col="grey",lty=2,xpd=NA)
	if(grid.label){text(x=rep(xlim[1]-1,length(grid)),y=grid,labels=prettyNum(grid,big.mark=","),cex=cex.axis, adj=1,xpd=NA)}
	# add year axis
	xlab.vals <- xlim[1]:xlim[2]
	xlab.labels <- pretty(xlab.vals ,n=length(xlab.vals)/4)
	xlab.labels <- xlab.labels[ xlab.labels %in% xlab.vals]
	axis(side=1,at=xlab.labels ,labels = xlab.labels ,cex.axis=cex.axis)

	#calc and plot moving avg
	if(!is.null(ma)){lines(x, filter(y,filter=rep(1/ma,ma) ,sides=1),col="red",lwd=2,lty=1)}

		lines(x,y, type="l",col="blue",lwd=2,xpd=TRUE) # overplot the grid lines

	if(minmaxpts==TRUE){
		# calc, plot, and label max point
		points(x[y==max(y,na.rm=TRUE)],y[y==max(y,na.rm=TRUE)],col="dark blue",pch=21,bg="green",lwd=1.2,cex=1.5)
		text(x[y==max(y,na.rm=TRUE)],y[y==max(y,na.rm=TRUE)] ,labels=paste(prettyNum(round(y[y==max(y,na.rm=TRUE)],2),big.mark=",")," (",x[y==max(y,na.rm=TRUE)],")",sep=""),pos=2,cex=1.1,xpd=NA)

		# calc, plot, and label min point
		#points(x[y==min(y,na.rm=TRUE)],y[y==min(y,na.rm=TRUE)],col="dark blue",pch=21,bg="red",lwd=1.2,cex=1.5)
		#text(x[y==min(y,na.rm=TRUE)],y[y==min(y,na.rm=TRUE)] ,labels=paste(prettyNum(round(y[y==min(y,na.rm=TRUE)],2),big.mark=",")," (",x[y==min(y,na.rm=TRUE)],")",sep=""),pos=2,cex=1.1,xpd=NA)
		} # end if minmax pts =TRUE
} # end ts.plot function


