# specify custom function for the ranking plot

ranking.plot <- function(source.in=HI_Landings_Data1, var.tmp = "Sold", var.label = "Sold (lb)", avg.use = "wt.Avg",num.units=12,x.lim.in=NULL,flag=NULL){


	tmp.summary <- factor.summary(var.use =var.tmp  , wt.use = "Sold", source.mat = source.in )

	if(is.null(x.lim.in)){x.lim.use <- max(tmp.summary[,"Max"])}
	if(!is.null(x.lim.in)){x.lim.use <- x.lim.in}

	tick.loc <- num.units:(num.units-dim(tmp.summary)[1]+1)
	options(scipen=100000)
	plot(1:10,1:10, xlim=c(0,x.lim.use),ylim=c(1,num.units), main=var.label,type="n",bty="n",axes=FALSE,xlab= "",ylab="",cex.lab=1.5)
	
	axis(2,at=tick.loc ,labels=tmp.summary[,1],las=1,lwd=1,line=1)

	x.ticks <- pretty(c(0,x.lim.use),n=5)
	x.ticks <- x.ticks[x.ticks<x.lim.use]
	
	if(max(x.ticks)<=10^3){x.ticks.labels <- x.ticks}
	if(max(x.ticks)>10^3){x.ticks.labels <- paste(x.ticks/10^3,"k",sep="")}
	if(max(x.ticks)>10^4){x.ticks.labels <- paste(x.ticks/10^4,"*10k",sep="")}
	if(max(x.ticks)>10^5){x.ticks.labels <- paste(x.ticks/10^5,"*100k",sep="")}
	if(max(x.ticks)>10^6){x.ticks.labels <- paste(x.ticks/10^6,"M",sep="")}

	axis(1,at=x.ticks,labels=x.ticks.labels)
	
	if(!is.null(flag)){
			flag.idx <- tmp.summary[,1]==flag
			abline(h=c(tick.loc[flag.idx]+0.5,tick.loc[flag.idx]-0.5),col="tomato",lty=2,xpd=TRUE)
			}

	lines(tmp.summary[,avg.use],tick.loc,type="b",pch=19,cex=1.6,col="darkblue",xpd=NA)
	segments(tmp.summary[,"Min"],tick.loc,tmp.summary[,"Max"],tick.loc,col="darkblue")
	points(tmp.summary[,"Min"],tick.loc,pch="|",cex=0.8,col="darkblue")
	points(tmp.summary[,"Max"],tick.loc,pch="|",cex=0.8,col="darkblue")

		# custom legend
		leg.y <- num.units*0.1
		points(x.lim.use*.8,leg.y,pch=19,cex=1.6,col="darkblue")
		segments(x.lim.use*.6,leg.y,x.lim.use*.9,leg.y)
		points(x.lim.use*.6,leg.y,pch="|")
		points(x.lim.use*.9,leg.y,pch="|")
		text(x.lim.use*.75,leg.y*1.6,labels="Range and weighted avg \nacross years",cex=.9)

}


# extract and summarize data for 1 variable,(incl weighted avg with weights based on user-spec)

# 

na.chk <- function(x){out <- TRUE; if(sum(!is.na(x))==0){out<-FALSE};return(out)}

factor.summary <- function(var.use = "Sold", wt.use = "Sold", source.mat = HI_Landings_Data1){

# filter out any records without any data in any of the rows
filter.list <- aggregate(source.mat[,var.use],by=list(source.mat[,"Species Label Short"]),FUN=na.chk)
filter.list <- as.vector(filter.list[filter.list[,2],1])
source.mat <- source.mat[source.mat[,"Species Label Short"] %in% filter.list,]
#print(source.mat)

tmp.mat <- aggregate(source.mat[,var.use],by=list(source.mat[,"Species Label Short"]),FUN=min,na.rm=TRUE)
dimnames(tmp.mat)[[2]] <- c("Species Label Short","Min")
tmp.mat <- cbind(tmp.mat,Med=aggregate(source.mat[,var.use],by=list(source.mat[,"Species Label Short"]),FUN=median,na.rm=TRUE)[,2])
tmp.mat <- cbind(tmp.mat,wt.Avg=NA)
tmp.mat <- cbind(tmp.mat,Avg=aggregate(source.mat[,var.use],by=list(source.mat[,"Species Label Short"]),FUN=mean,na.rm=TRUE)[,2])
tmp.mat <- cbind(tmp.mat,Max=aggregate(source.mat[,var.use],by=list(source.mat[,"Species Label Short"]),FUN=max,na.rm=TRUE)[,2])

# any way of doing weighted avg within aggregate?
grp.sums <- aggregate(source.mat[,wt.use],by=list(source.mat[,"Species Label Short"]),FUN=sum,na.rm=TRUE)
for(spec in tmp.mat[,1]){ 
	tmp.wt.vals <- source.mat[source.mat[,"Species Label Short"]==spec,wt.use]
	tmp.wts <- tmp.wt.vals / grp.sums[grp.sums[,"Group.1"]==spec,"x"]
	tmp.vals <- source.mat[source.mat[,"Species Label Short"]==spec,var.use]
	tmp.mat[tmp.mat[,1]==spec ,"wt.Avg"] <- sum(tmp.vals*tmp.wts,na.rm=TRUE)
	}

tmp.mat <- tmp.mat[order(tmp.mat[,"wt.Avg"],decreasing=TRUE),]

return(tmp.mat)

}
