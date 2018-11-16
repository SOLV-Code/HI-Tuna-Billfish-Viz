# SCRIPT TO CREATE VARIOUS TIME SERIES PLOTS

print("")
print("")
print("")
print("------------------------------------------------------------------------------")
print("STARTING SCRIPT 2")

# read in latest version of the data
source("1_GetData_Script.R")

# check the data
print(head(HI_Landings_Data1))

# read in the latest version of all the functions
fn.list <- dir("../Shiny App/Functions")
for(fn in fn.list){print(paste("sourcing", fn)) ; source(paste("../Shiny App/Functions",fn,sep="/"))}


# do some summaries of the data

data.yrs <- min(HI_Landings_Data1[,"Year"]):max(HI_Landings_Data1[,"Year"])
print(data.yrs)
# Note: missing years were included as NA in the data processing step, 
# so there are equal number of obs for each species

dimnames(HI_Landings_Data1)[[2]]

species.list <- sort(unique(HI_Landings_Data1[,"Species"]))
print(species.list)

species.info.labels <- c("Species","FisheryGroup", "Genus" ,"SpeciesName","SpeciesLabelShort","SpeciesLabelLong")
species.lookup <- HI_Landings_Data1[ HI_Landings_Data1[,"Year"]==min(data.yrs) ,species.info.labels ]      


####################################################################################################################
# EXAMPLE 1: FANCY TIME SERIES PLOTS

# test the time series plot
par(omi=c(0.5,0.5,0.5,0.5) , mai=c(0.7,1.3,0.4,0.3))
ts.plot(data.yrs , HI_Landings_Data1[HI_Landings_Data1[,"Species"]==species.list[1],"Sold"] ,
	 xlim=range(data.yrs), ma=4,minmaxpts=TRUE)
title(main=paste(species.lookup[species.lookup[,"Species"]==species.list[1],"Species Label Long"],"Sold (lb)", sep=" -- "),cex.main=1.6)

# ----------------------------------------
# Plot 1a: create pdf handout with 1 page per species, and 3 panels showing SOLD, ADJ VALUE, ADJ PRICE PER LB
# ----------------------------------------

# open the pdf file as a plotting device
# note: onefile = TRUE creates a new page in the same pdf file whenever the current device is full
if(!dir.exists("OUTPUT/Examples")){dir.create("OUTPUT/Examples")}
if(!dir.exists("OUTPUT/Examples/Time Series Examples")){dir.create("OUTPUT/Examples/Time Series Examples")}

pdf("OUTPUT/Examples/Time Series Examples/Example_1a_FancyTimeSeries_Handout.pdf",width=8.5, height=11,onefile=TRUE)

# specify margins 
par(omi=c(0.5,0.5,0.5,0.5) , mai=c(0.7,0.6,0.4,0.3))

# divide the plotting device into 3 panels 
layout(matrix(c(1:3),ncol=1))



ex1a.var.list <- c("Sold","AdjValue","AdjPricePerLb")

for(fish.grp in sort(unique(species.lookup[,"FisheryGroup"]))){
	
	tmp.spec.list <- sort(species.lookup[species.lookup[,"FisheryGroup"]==fish.grp,"Species"])
	for(spec in tmp.spec.list){

	for(var.use in ex1a.var.list){
		ts.plot(data.yrs , HI_Landings_Data1[HI_Landings_Data1[,"Species"]==spec,var.use] ,
			xlim=range(data.yrs), ma=4,minmaxpts=TRUE)
		title(main= var.use,,cex.main=1.5,col="darkblue")
		}
	title(main=paste(species.lookup[species.lookup[,"Species"]==spec,"SpeciesLabelLong"],
		paste("(",species.lookup[species.lookup[,"Species"]==spec,"SpeciesName"],")",sep=""), sep=" -- ")
		,cex.main=1.8,outer=TRUE,col="darkblue")

	}}


dev.off()




####################################################################################################################
# EXAMPLE 2: PERCENT RANK PLOTS

#test the percent rank plots
par(mfrow=c(2,1)); par(mai=c(1,1.2,1,1))
perc.rank.plot(HI_Landings_Data1[HI_Landings_Data1[,"Species"]==species.list[1],"Sold"], ma=4,type="fancy", yrs.lab=range(data.yrs))
perc.rank.plot(HI_Landings_Data1[HI_Landings_Data1[,"Species"]==species.list[1],"Sold"], type="spark")


# ----------------------------------------
# Plot 2a: create pdf handout with 1 page per species, and 3 panels showing SOLD, ADJ VALUE, ADJ PRICE PER LB
# ----------------------------------------
# open the pdf file as a plotting device
# note: onefile = TRUE creates a new page in the same pdf file whenever the current device is full
pdf("OUTPUT/Examples/Time Series Examples/Example_2a_FancyPercRank_Handout.pdf",width=8.5, height=11,onefile=TRUE)

# specify margins 
par(omi=c(0.5,0.5,0.5,0.5) , mai=c(0.7,0.6,0.4,0.3))

# divide the plotting device into 3 panels 
layout(matrix(c(1:3),ncol=1))



ex2a.var.list <- c("Sold","AdjValue","AdjPricePerLb")

for(fish.grp in sort(unique(species.lookup[,"FisheryGroup"]))){
	
	tmp.spec.list <- sort(species.lookup[species.lookup[,"FisheryGroup"]==fish.grp,"Species"])
	for(spec in tmp.spec.list){

	for(var.use in ex2a.var.list){
		perc.rank.plot(HI_Landings_Data1[HI_Landings_Data1[,"Species"]==spec,var.use],
			 ma=4,type="fancy", yrs.lab=range(data.yrs))
		title(main= var.use,,cex.main=1.5,col="darkblue")
		}
	title(main=paste(species.lookup[species.lookup[,"Species"]==spec,"SpeciesLabelLong"],
		paste("(",species.lookup[species.lookup[,"Species"]==spec,"SpeciesName"],")",sep=""), sep=" -- ")
		,cex.main=1.8,outer=TRUE,col="darkblue")

	}}


dev.off()


# ----------------------------------------
# Plot 2b: create pdf handout of percent rank sparklines with 1 page per fishery group, and 3 columns showing SOLD, ADJ VALUE, ADJ PRICE PER LB
# ----------------------------------------
# open the pdf file as a plotting device
# note: onefile = TRUE creates a new page in the same pdf file whenever the current device is full
pdf("OUTPUT/Examples/Time Series Examples/Example_2b_SparklinePercRank_Handout.pdf",width=8.5, height=11,onefile=TRUE)

# divide the plotting device into 21 panels (7x3)
layout(matrix(c(1:21),ncol=3,byrow=TRUE))

ex2b.var.list <- c("Sold","AdjValue","AdjPricePerLb")
ex2b.var.labels <- c("Sold (Lb)","Value (Adj to 2015$)","$/Lb (Adj to 2015$)")

for(fish.grp in sort(unique(species.lookup[,"FisheryGroup"]))){

	# specify margins (and trigger a new page)
	par(omi=c(0.3,0.3,0.7,0.3) , mai=c(0.2,0.2,0.2,0.2))	

	tmp.spec.list <- sort(species.lookup[species.lookup[,"FisheryGroup"]==fish.grp,"Species"])
	for(spec in tmp.spec.list){

	for(var.use in ex2b.var.list){
		perc.rank.plot(HI_Landings_Data1[HI_Landings_Data1[,"Species"]==spec,var.use],type="spark")
		abline(h=0.7,col="lightgrey",xpd=NA)
		title(sub= ex2b.var.labels[ex2b.var.list %in% var.use],col.sub="darkblue",xpd=NA,line=-0.1) # label the variable
			if(var.use == ex2b.var.list[2]){ # add species label above 2nd of 3 plots in a row
				title(main=species.lookup[species.lookup[,"Species"]==spec,"SpeciesLabelLong"]) }
			if(var.use == ex2b.var.list[3]& spec == tmp.spec.list[1]){ # add plot explanation in top right corner
				title(main="Plots show percent rank\nas deviations from median",line=+3.5,xpd=NA,cex.main=1.1,col.main="tomato") }
			
		}
		title(main=fish.grp,outer=TRUE,cex.main=2.5,col.main="darkblue")
	}
}
dev.off()



# ----------------------------------------
# Plot 2c: create pdf handout of regular sparklines with 1 page per fishery group, and 3 columns showing SOLD, ADJ VALUE, ADJ PRICE PER LB
# ----------------------------------------
# open the pdf file as a plotting device
# note: onefile = TRUE creates a new page in the same pdf file whenever the current device is full
pdf("OUTPUT/Examples/Time Series Examples/Example_2c_SparklineRegular_Handout.pdf",width=8.5, height=11,onefile=TRUE)

# divide the plotting device into 21 panels (7x3)
layout(matrix(c(1:21),ncol=3,byrow=TRUE))

ex2c.var.list <- c("Sold","AdjValue","AdjPricePerLb")
ex2c.var.labels <- c("Sold (Lb)","Value (Adj to 2015$)","$/Lb (Adj to 2015$)")

for(fish.grp in sort(unique(species.lookup[,"FisheryGroup"]))){

	# specify margins (and trigger a new page)
	par(omi=c(0.3,0.3,0.7,0.3) , mai=c(0.2,0.2,0.2,0.2))	

	tmp.spec.list <- sort(species.lookup[species.lookup[,"FisheryGroup"]==fish.grp,"Species"])
	for(spec in tmp.spec.list){

	for(var.use in ex2c.var.list){
		tmp.vals <- HI_Landings_Data1[HI_Landings_Data1[,"Species"]==spec,var.use]/max(HI_Landings_Data1[HI_Landings_Data1[,"Species"]==spec,var.use],na.rm=TRUE)
		plot(data.yrs,tmp.vals,type="l",bty="n", axes=FALSE,xlab="",ylab="",col="darkblue",lwd=2,ylim=c(0,1))
		abline(h=par("usr")[4]*1.2,col="lightgrey",xpd=NA)
		title(sub= ex2c.var.labels[ex2b.var.list %in% var.use],col.sub="darkblue",xpd=NA,line=-0.1) # label the variable
			if(var.use == ex2c.var.list[2]){ # add species label above 2nd of 3 plots in a row
				title(main=species.lookup[species.lookup[,"Species"]==spec,"SpeciesLabelLong"]) }
			if(var.use == ex2c.var.list[3]& spec == tmp.spec.list[1]){ # add plot explanation in top right corner
				title(main="Plots show time series as is",line=+3.5,xpd=NA,cex.main=1.1,col.main="tomato") }
			
		}
		title(main=fish.grp,outer=TRUE,cex.main=2.5,col.main="darkblue")
	}
}
dev.off()


# ----------------------------------------
# Plot 2d: create pdf handout of regular sparklines with 1 page per fishery group, and 3 columns showing SOLD, ADJ VALUE, ADJ PRICE PER LB
# ----------------------------------------
# open the pdf file as a plotting device
# note: onefile = TRUE creates a new page in the same pdf file whenever the current device is full
pdf("OUTPUT/Examples/Time Series Examples/Example_2d_SparklineLogTransform_Handout.pdf",width=8.5, height=11,onefile=TRUE)

# divide the plotting device into 21 panels (7x3)
layout(matrix(c(1:21),ncol=3,byrow=TRUE))

ex2d.var.list <- c("Sold","AdjValue","AdjPricePerLb")
ex2d.var.labels <- c("Sold (Lb)","Value (Adj to 2015$)","$/Lb (Adj to 2015$)")

for(fish.grp in sort(unique(species.lookup[,"FisheryGroup"]))){

	# specify margins (and trigger a new page)
	par(omi=c(0.3,0.3,0.7,0.3) , mai=c(0.2,0.2,0.2,0.2))	

	tmp.spec.list <- sort(species.lookup[species.lookup[,"FisheryGroup"]==fish.grp,"Species"])
	for(spec in tmp.spec.list){

	for(var.use in ex2d.var.list){
		tmp.data <- log(HI_Landings_Data1[HI_Landings_Data1[,"Species"]==spec,var.use])
		tmp.vals <- tmp.data /max(tmp.data ,na.rm=TRUE)
		plot(data.yrs,tmp.vals,type="l",bty="n", axes=FALSE,xlab="",ylab="",col="darkblue",lwd=2,ylim=c(0,1))
		abline(h=par("usr")[4]*1.2,col="lightgrey",xpd=NA)
		title(sub= ex2d.var.labels[ex2b.var.list %in% var.use],col.sub="darkblue",xpd=NA,line=-0.1) # label the variable
			if(var.use == ex2d.var.list[2]){ # add species label above 2nd of 3 plots in a row
				title(main=species.lookup[species.lookup[,"Species"]==spec,"SpeciesLabelLong"]) }
			if(var.use == ex2d.var.list[3]& spec == tmp.spec.list[1]){ # add plot explanation in top right corner
				title(main="Plots show log-transformed\ntime series",line=+3.5,xpd=NA,cex.main=1.1,col.main="tomato") }
			
		}
		title(main=fish.grp,outer=TRUE,cex.main=2.5,col.main="darkblue")
	}
}
dev.off()





