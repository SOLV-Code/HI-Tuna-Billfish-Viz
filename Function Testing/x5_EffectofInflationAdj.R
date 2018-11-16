# SCRIPT TO CREATE VARIOUS TIME SERIES PLOTS

print("")
print("")
print("")
print("------------------------------------------------------------------------------")
print("STARTING SCRIPT 5")

# read in latest version of the data
# commented out, because only need to run if any database changes
# NOTE: this step requires 32-bit R (due to RODBC package)
# source("1_GetData_Script.R")

# check the data
#print(HI_Landings_Data1)

# read in the latest version of all the functions
fn.list <- dir("Functions")
for(fn in fn.list){source(paste("Functions",fn,sep="/"))}


# do some summaries of the data

data.yrs <- min(HI_Landings_Data1[,"Year"]):max(HI_Landings_Data1[,"Year"])
print(data.yrs)
# Note: missing years were included as NA in the data processing step, 
# so there are equal number of obs for each species

dimnames(HI_Landings_Data1)[[2]]

species.list <- sort(unique(HI_Landings_Data1[,"Species"]))
print(species.list)

species.info.labels <- c("Species","Fishery Group", "Genus" ,"Species Name","Species Label Short","Species Label Long")
species.lookup <- HI_Landings_Data1[ HI_Landings_Data1[,"Year"]==min(data.yrs) ,species.info.labels ]      


####################################################################################################################
# EXAMPLE 5: FANCY TIME SERIES PLOTS







# read in the latest version of all the functions
fn.list <- dir("Functions")
for(fn in fn.list){source(paste("Functions",fn,sep="/"))}




# test the time series comparison plot
par(omi=c(0.5,0.5,0.5,0.5) , mai=c(0.7,1.3,0.4,0.3))
ts.comp.plot(x=data.yrs , 
		y1= HI_Landings_Data1[HI_Landings_Data1[,"Species"]==species.list[1],"AdjValue"] ,
		y2= HI_Landings_Data1[HI_Landings_Data1[,"Species"]==species.list[1],"Value"] ,
		xlim=range(data.yrs),minmaxpts=TRUE,
		y1.label="Adj. Value (2015$)",y2.label="Annual Value")


title(main=species.lookup[species.lookup[,"Species"]==species.list[1],"Species Label Long"],cex.main=1.6)






# ----------------------------------------
# Plot 5a: create pdf handout with 1 page per species, and 3 rows of panels: 
# top: Value and Adj value
# mid: $/lb and adj $/lb
# bottom: 3 col - Infl adj all years, since 1948, last 20 yrs
# ----------------------------------------

# open the pdf file as a plotting device
# note: onefile = TRUE creates a new page in the same pdf file whenever the current device is full
pdf("OUTPUT/Examples/Effect of Inflation Adjustment/Example_5a_TimeSeriesComparison_Handout.pdf",width=8.5, height=11,onefile=TRUE)


infl.yrs <- list(all=1913:2015,match=sort(unique(HI_Landings_Data1[,"Year"]) ),recent=1985:2015 )


# divide the plotting device into 9 panels (with top 2 rows merged)
layout(matrix(c(1,1,
			2,2,
			3,3,
			4,5), byrow=TRUE,ncol=2))

for(fish.grp in sort(unique(species.lookup[,"Fishery Group"]))){
	
	tmp.spec.list <- sort(species.lookup[species.lookup[,"Fishery Group"]==fish.grp,"Species"])
	for(spec in tmp.spec.list){
	# specify margins (and reset plotting page due to omi)
	par(omi=c(0.5,0.5,0.5,0.5) , mai=c(0.7,0.6,0.4,0.3))
	ts.comp.plot(x=data.yrs , 
		y1= HI_Landings_Data1[HI_Landings_Data1[,"Species"]==spec ,"AdjValue"] ,
		y2= HI_Landings_Data1[HI_Landings_Data1[,"Species"]==spec ,"Value"] ,
		xlim=range(data.yrs),minmaxpts=TRUE,
		y1.label="Adj. Value (2015$)",y2.label="Annual Value")

	title(main="Value (US$)",cex.main=1.3)

	title(main=species.lookup[species.lookup[,"Species"]==spec,"Species Label Long"],cex.main=1.8,outer=TRUE,col.main="darkblue")
	title(main=species.lookup[species.lookup[,"Species"]==spec,"Species Name"],line=-0.3,font.main=3,cex.main=1.8,outer=TRUE,col.main="darkblue")

	ts.comp.plot(x=data.yrs , 
		y1= HI_Landings_Data1[HI_Landings_Data1[,"Species"]==spec ,"AdjPricePerLb"] ,
		y2= HI_Landings_Data1[HI_Landings_Data1[,"Species"]==spec ,"PricePerLb"] ,
		xlim=range(data.yrs),minmaxpts=TRUE,
		y1.label="$/lb (2015$)",y2.label="$/lb (Annual)")	
	title(main="Price / Pound",cex.main=1.3)

	
	
	
	ts.plot(infl.yrs$match,US_Infl[US_Infl[,"Year"] %in% infl.yrs$match,"Scalar"],xlim=range(data.yrs),minmaxpts=FALSE)
	title(main="Inflation Adjustment (Based on US$ CPI)",cex.main=1.3)


	par(mai=c(0.2,0.6,0.2,0.2))
	
	# should convert this into a custom plot function, but for now just copied it for next plot
	plot(infl.yrs$all,US_Infl[US_Infl[,"Year"] %in% infl.yrs$all,"Scalar"],type="l",bty="n", axes=FALSE ,xlab="",ylab="",col="blue",lwd=2)
	title(main=paste("Infl. Adj.",min(infl.yrs$all), "to",max(infl.yrs$all)),cex.main=1.3)
	pts.idx <- c(1,length(infl.yrs$all))
	pts.x <- infl.yrs$all[pts.idx]
	pts.y <- US_Infl[US_Infl[,"Year"] %in% infl.yrs$all,"Scalar"][pts.idx ]
	abline(v=seq(1910,2010,by=20),col="red",lty=2,lwd=0.5)
	text(seq(1910,2010,by=20),par("usr")[3],seq(1910,2010,by=20),col="red",xpd=TRUE)
	lines(infl.yrs$all,US_Infl[US_Infl[,"Year"] %in% infl.yrs$all,"Scalar"],type="l",col="blue",lwd=2)# repeat to paint over gridlines
	points(pts.x,pts.y,col="blue",pch=21,bg="lightgray",lwd=1.2,cex=1.5)
	text(pts.x,pts.y, paste(prettyNum(pts.y,2,big.mark=","),"\n(",pts.x,")",sep=""),pos=3,cex=1.1,xpd=NA)
	

	plot(infl.yrs$recent,US_Infl[US_Infl[,"Year"] %in% infl.yrs$recent,"Scalar"],type="l",bty="n", axes=FALSE,xlab="",ylab="",col="blue",lwd=2)
	title(main=paste("Infl. Adj.",min(infl.yrs$recent), "to",max(infl.yrs$recent)),cex.main=1.3)
	pts.idx <- c(1,length(infl.yrs$recent))
	pts.x <- infl.yrs$recent[pts.idx]
	pts.y <- US_Infl[US_Infl[,"Year"] %in% infl.yrs$recent,"Scalar"][pts.idx ]
	abline(v=seq(1910,2010,by=20),col="red",lty=2,lwd=0.5); text(seq(1910,2010,by=20),par("usr")[3],seq(1910,2010,by=20),col="red",xpd=TRUE)
	lines(infl.yrs$recent,US_Infl[US_Infl[,"Year"] %in% infl.yrs$recent,"Scalar"],type="l",col="blue",lwd=2)# repeat to paint over gridlines
	points(pts.x,pts.y,col="blue",pch=21,bg="lightgray",lwd=1.2,cex=1.5)
	text(pts.x,pts.y, paste(prettyNum(pts.y,2,big.mark=","),"\n(",pts.x,")",sep=""),pos=3,cex=1.1,xpd=NA)
	

		} #end looping through species



	}


dev.off()



