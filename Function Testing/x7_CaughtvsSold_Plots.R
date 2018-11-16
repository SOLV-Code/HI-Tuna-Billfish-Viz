# SCRIPT TO CREATE PLOTS THAT DIFFERENCE BETWEEN ESTIMATED CATCH AND REPORTED LANDINGS OVER TIME

print("")
print("")
print("")
print("------------------------------------------------------------------------------")
print("STARTING SCRIPT 7")

# read in latest version of the data
# commented out, because only need to run if any database changes
# NOTE: this step requires 32-bit R (due to RODBC package)
# source("1_GetData_Script.R")

# check the data
#print(HI_Landings_Data1)

# read in the latest version of all the functions
fn.list <- dir("Functions")
for(fn in fn.list){source(paste("Functions",fn,sep="/"))}

#############################################
# do some summary checks of the data 


data.yrs <- min(HI_Landings_Data1[,"Year"]):max(HI_Landings_Data1[,"Year"])
print(data.yrs)
# Note: missing years were included as NA in the data processing step, 
# so there are equal number of obs for each species

dimnames(HI_Landings_Data1)[[2]]

species.list <- sort(unique(HI_Landings_Data1[,"Species"]))
print(species.list)

species.info.labels <- c("Species","Fishery Group", "Genus" ,"Species Name","Species Label Short","Species Label Long")
species.lookup <- HI_Landings_Data1[ HI_Landings_Data1[,"Year"]==min(data.yrs) ,species.info.labels ]      



################################
# Example 7a: compare time series of caught and sold estimates


pdf("OUTPUT/Examples/Stacked Bar Examples/Example7a_TimeSeriesofCaughtandSold.pdf",width=11,height=8.5,onefile=TRUE)
par(omi=c(0.5,0.5,0.5,0.5) , mai=c(0.7,1.3,0.4,0.3))
par(mfrow=c(2,1))
for(species in species.list){


	ts.comp.plot(x=data.yrs , 
		y1= HI_Landings_Data1[HI_Landings_Data1[,"Species"]==species,"Sold"],
		y2= HI_Landings_Data1[HI_Landings_Data1[,"Species"]==species,"Caught"] ,
		xlim=range(data.yrs),minmaxpts=TRUE,
		y1.label="Sold Estimate(lb)",y2.label="Caught Estimate(lb)")

		title(main=species.lookup[species.lookup[,"Species"]==species,"Species Label Long"],cex.main=1.6)

}

dev.off()



################################
# Example 7b: Plot Cumulative Discrepancy - All years Across Species
# NOTE: NAs are being dropped!

tmp.mat <- matrix(NA,ncol=4,nrow=length(species.list),dimnames=list(species.list,c("Caught","Sold","Diff","PercDiff")))

for(species in species.list){  
	tmp.mat[species,"Caught"] <- sum(HI_Landings_Data1[HI_Landings_Data1[,"Species"]==species,"Caught"],na.rm=TRUE) /10^6
	tmp.mat[species,"Sold"] <- sum(HI_Landings_Data1[HI_Landings_Data1[,"Species"]==species,"Sold"],na.rm=TRUE)  /10^6

}

tmp.mat[,"Diff"]<- tmp.mat[,"Caught"] - tmp.mat[,"Sold"]
tmp.mat[,"PercDiff"] <- round(100*(tmp.mat[,"Diff"]/ tmp.mat[,"Sold"]),1)
write.csv(tmp.mat,file="OUTPUT/Examples/Stacked Bar Examples/CumulDiscr_AllYears_SummaryTable.csv")

print(tmp.mat)

pdf("OUTPUT/Examples/Stacked Bar Examples/Example7b_StackedBar0fCaughtandSold_AllYears.pdf",width=11,height=8.5,onefile=TRUE)
par(omi=c(0.5,0.5,0.5,0.5) , mai=c(0.7,1.3,0.4,0.3))

stacked.bar.comp.plot(tmp.mat[dim(tmp.mat)[1]:1,],var.larger="Caught",var.smaller="Sold",
		title.in=paste("Cumulative Landing Estimates (Mill Lb) -",min(data.yrs),"to",max(data.yrs)),x.lim=NULL)

legend("topright",legend=c("Sold","Caught-Sold"),density=c(NA,25),fill=c("dodgerblue","red"),bty="n")

dev.off()


################################
# Example 7b: Plot Cumulative Discrepancy - By Species and Decade
# NOTE: NAs are being dropped!

# use this object created earlier
decades.list


tmp.array <- array(NA,dim=c(length(names(decades.list)),4,length(species.list)),dimnames=list(names(decades.list),c("Caught","Sold","Diff","PercDiff"),species.list))



for(species in species.list){
	print("--------");print(species)

		for(decade in names(decades.list)){
			
			subset.idx <- HI_Landings_Data1[,"Species"]==species & HI_Landings_Data1[,"Year"] %in% decades.list[[decade]]
			tmp.array[decade,"Caught",species] <- sum(HI_Landings_Data1[ subset.idx  ,"Caught"],na.rm=TRUE) /10^6
			tmp.array[decade,"Sold",species] <- sum(HI_Landings_Data1[ subset.idx  ,"Sold"],na.rm=TRUE) /10^6
			
			tmp.array[decade,"Diff",species] <- tmp.array[decade,"Caught",species] - tmp.array[decade,"Sold",species] 
			tmp.array[decade,"PercDiff",species] <- round(100*(tmp.array[decade,"Diff",species] / tmp.array[decade,"Sold",species] ),1)

	}}


print(tmp.array)

out.mat <- cbind(dimnames(tmp.array)[[3]][1],dimnames(tmp.array)[[1]],tmp.array[,,1])
for(species in species.list[-1]){
		out.mat <- rbind(out.mat ,  cbind(species,dimnames(tmp.array)[[1]],tmp.array[,,species]))
	}
dimnames(out.mat) <- list(NULL,c("Species","Decade","Caught","Sold","Diff","PercDiff"))
print(out.mat)
write.csv(out.mat,file="OUTPUT/Examples/Stacked Bar Examples/CumulDiscr_BySpeciesAndDecade_SummaryTable.csv")





pdf("OUTPUT/Examples/Stacked Bar Examples/Example7c_StackedBar0fCaughtandSold_BySpeciesAndDecade.pdf",width=11,height=8.5,onefile=TRUE)
par(omi=c(0.5,0.5,0.5,0.5) , mai=c(0.7,1.3,0.4,0.3))
par(mfrow=c(2,2))

for(species in species.list){
	stacked.bar.comp.plot(tmp.array[dim(tmp.array[,,species])[1]:1,,species],var.larger="Caught",var.smaller="Sold",
		title.in=paste(species,"Landings by Decade (Mill Lb)"),x.lim=NULL)
	legend("topright",legend=c("Sold","Caught-Sold"),density=c(NA,25),fill=c("dodgerblue","red"),bty="n")
}



dev.off()












