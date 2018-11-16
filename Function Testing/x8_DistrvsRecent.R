# SCRIPT TO CREATE COMPARES RECENT OBSERVATIONS AGAINST DISTRIBUTIONS FROM EARLIER PERIODS

print("")
print("")
print("")
print("------------------------------------------------------------------------------")
print("STARTING SCRIPT 8")

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

#probs.use <- c(0,.25,.5,.75,1)
#probs.labels <-c("min",paste("p",probs.use[2:4]*100,sep=""),"max")

probs.use <- c(0.1,.25,.5,.75,.9)
probs.labels <- paste("p",probs.use*100,sep="")



# previously created object
decades.list






################################################################################
# Example 8a: Overall distribution, Decadal distribution, and Last 4 years

var.list <- c("Sold", "AdjValue","AdjPricePerLb")
var.label.list <- c("Sold (Mill Lb)","Value (Mill 2015$)","$/lb (2015$)")
var.scale <-c(10^6,10^6,1)
recent.yrs<- (max(data.yrs)-3):max(data.yrs)


pdf("OUTPUT/Examples/Distribution vs Recent/Example8a_DistrvsRecent.pdf",width=11,height=8.5,onefile=TRUE)

for(species in species.list){

print("-------------------")
print(species)
par(omi=c(0.5,0.5,0.5,0.5) , mai=c(1.3,0.7,0.4,0.3))
par(mfrow=c(2,2))

for(var.idx in 1:length(var.list)){



var.use <- var.list[var.idx]
var.label <- var.label.list[var.idx]
print(var.use)

# filter data
tmp.all <- quantile(HI_Landings_Data1[HI_Landings_Data1[,"Species"]==species ,var.use],probs.use,na.rm=TRUE)/var.scale[var.idx]
names(tmp.all) <- probs.labels
tmp.periods <- matrix(NA,ncol=length(probs.use),nrow=length(names(decades.list)),
			dimnames=list(names(decades.list),probs.labels))


for(decade in names(decades.list)){
		decade.idx <- HI_Landings_Data1[,"Species"]==species  & HI_Landings_Data1[,"Year"] %in% decades.list[[decade]] 		
		tmp.periods[decade,] <-  quantile(HI_Landings_Data1[decade.idx ,var.use],probs.use,na.rm=TRUE)/var.scale[var.idx]
		}

recent.idx <- HI_Landings_Data1[,"Species"]==species  & HI_Landings_Data1[,"Year"] %in% recent.yrs 
tmp.recent <- HI_Landings_Data1[recent.idx,var.use]/var.scale[var.idx]
names(tmp.recent)<-recent.yrs

# check objects
print(tmp.all)
print(tmp.periods)
print(tmp.recent)


distrvsrecent.plot(tmp.all,tmp.periods,tmp.recent,val.label=var.label)
title(paste(var.label))

} # end looping through var

title(main=species.lookup[species.lookup[,"Species"]==species,"Species Label Long"],cex.main=1.8,outer=TRUE,col.main="darkblue")
title(main=species.lookup[species.lookup[,"Species"]==species,"Species Name"],line=-0.3,font.main=3,cex.main=1.8,outer=TRUE,col.main="darkblue")

} # end looping through species
dev.off()



################################################################################
# Example 8b: Overall distribution, Decadal distribution since 90s, and Last 6 years

var.list <- c("Sold", "AdjValue","AdjPricePerLb")
var.label.list <- c("Sold (Mill Lb)","Value (Mill 2015$)","$/lb (2015$)")
var.scale <-c(10^6,10^6,1)
recent.yrs <- 2010:2015



for(var.idx in 1:length(var.list)){

var.use <- var.list[var.idx]
var.label <- var.label.list[var.idx]

pdf(paste("OUTPUT/Examples/Distribution vs Recent/Example8b_DistrvsRecent_v2_",var.use,".pdf",sep=""),width=11, height=8.5,onefile=TRUE)

# divide the plotting device into 6 panels (2x3)

layout(matrix(c(1:6),ncol=3,byrow=TRUE))

for(fish.grp in sort(unique(species.lookup[,"Fishery Group"]))){

	# specify margins (and trigger a new page)
	par(omi=c(0.5,0.5,0.5,0.5) , mai=c(1,0.7,.5,0.3))

	tmp.spec.list <- sort(species.lookup[species.lookup[,"Fishery Group"]==fish.grp,"Species"])
	for(species in tmp.spec.list){
	
			print(var.use); print(fish.grp);print(species)
	
			# filter data
			tmp.all <- quantile(HI_Landings_Data1[HI_Landings_Data1[,"Species"]==species ,var.use],probs.use,na.rm=TRUE)/var.scale[var.idx]
			names(tmp.all) <- probs.labels
			tmp.periods <- matrix(NA,ncol=length(probs.use),nrow=length(c("Nineties","Aughts")),
					dimnames=list(c("Nineties","Aughts"),probs.labels))

			for(decade in c("Nineties","Aughts")){
				decade.idx <- HI_Landings_Data1[,"Species"]==species  & HI_Landings_Data1[,"Year"] %in% decades.list[[decade]] 		
				tmp.periods[decade,] <-  quantile(HI_Landings_Data1[decade.idx ,var.use],probs.use,na.rm=TRUE)/var.scale[var.idx]
				}

			recent.idx <- HI_Landings_Data1[,"Species"]==species  & HI_Landings_Data1[,"Year"] %in% recent.yrs 
			tmp.recent <- HI_Landings_Data1[recent.idx,var.use]/var.scale[var.idx]
			names(tmp.recent)<-recent.yrs
	
			distrvsrecent.plot(tmp.all,tmp.periods,tmp.recent,val.label=var.label)
			title(species.lookup[species.lookup[,"Species"]==species,"Species Label Long"])

			if(species == tmp.spec.list[1]){ # add plot explanation in top right corner
				title(main="Barplots: Line=median \nBar=25-75 %ile\nWhisker=10-90 %ile",line=+3.5,xpd=NA,cex.main=1.1,col.main="tomato") }
			
		} # end looping through species
		title(main=paste(fish.grp,"-",var.label),outer=TRUE,cex.main=2.5,col.main="darkblue")
		
		
	} # end looping through Fishery Group
	
dev.off()
	
	
} # end looping through variables





