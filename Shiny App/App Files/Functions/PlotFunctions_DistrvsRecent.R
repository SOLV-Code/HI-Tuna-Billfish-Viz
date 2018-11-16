
box.add <- function(X,at.x=1,box.width=1,fill.use="white",border.use="darkblue"){
		# X is a vector with 5 values (typically 10th,25th,50th,75th, and 90th percentiles of a distribution)
		segments(at.x,X[1],at.x,X[5],col=border.use)
		rect(at.x-box.width/2,X[2],at.x+box.width/2,X[4],col=fill.use,border=border.use)
		segments(at.x-(box.width/2)+0.1,X[3],at.x+(box.width/2)-0.1,X[3],col=border.use,lwd=3,lend=2)
			}


box.add.horiz <- function(X,at.y=1,box.width=1,fill.use="white",border.use="darkblue"){
		# X is a vector with 5 values (typically 10th,25th,50th,75th, and 90th percentiles of a distribution)
		segments(X[1],at.y,X[5],at.y,col=border.use)
		rect(X[2],at.y-box.width/2,X[4],at.y+box.width/2,col=fill.use,border=border.use)
		segments(X[3],at.y-(box.width/2)+0.1,X[3],at.y+(box.width/2)-0.1,,col=border.use,lwd=2,lend=2)
			}



distrvsrecent.plot <- function(X_all,X_periods,X_recent,val.label=NULL,start.loc=NULL,xlim.in=NULL,ylim.in=NULL){
# X_all:
# X_periods:
# X_recent:
# labels: a list with elements $all, $periods, $recent
# start.loc: x position for first element of each piece (first of the period bars, first of the recent obs)

if(is.null(xlim.in)){ xlim.use <- c(0, dim(X_periods)[1]+length(X_recent)+3)  }
if(!is.null(xlim.in)){xlim.use <- xlim.in}

if(is.null(ylim.in)){ ylim.use <- c(0,max(X_all,X_periods,X_recent,na.rm=TRUE))}
if(!is.null(ylim.in)){ylim.use <- ylim.in}

if(is.null(start.loc)){ start.loc.use <- c(1,3, 3+dim(X_periods)[1]+1)                            }
if(!is.null(start.loc)){ start.loc.use <- start.loc}
plot(1:10,1:10,xlim=xlim.use, ylim=ylim.use, axes=FALSE, xlab="", ylab=val.label,type="n")
#axis(1

# median of recent
abline(h=median(X_recent),col="red",lty=2,lwd=2)

box.add(X_all,at.x=start.loc.use[1],box.width=0.75,fill.use="dodgerblue",border.use="darkblue")
axis(1,at=start.loc.use[1],labels="All")

periods.x <- start.loc.use[2]:(start.loc.use[2]-1+dim(X_periods)[1])
for(i in 1:dim(X_periods)[1]){
	box.add(X_periods[i,],at.x=periods.x[i],box.width=0.75,fill.use="white",border.use="darkblue")
	}
axis(1,at=periods.x,labels=dimnames(X_periods)[[1]],las=2)
	
	
recent.x <- start.loc.use[3]:(start.loc.use[3]-1+length(X_recent))
lines(recent.x ,X_recent,type="b",pch=21,col="darkblue",bg="red",cex=2)
axis(1,at=recent.x,labels=names(X_recent),las=2)

val.axis.ticks <- pretty(c(0,ylim.use))
val.axis.ticks <- val.axis.ticks[val.axis.ticks<max(ylim.use)]

axis(2,at=val.axis.ticks ,labels=val.axis.ticks )




}