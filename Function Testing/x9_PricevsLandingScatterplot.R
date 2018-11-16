# SCRIPT TO CREATE SCATTERPLOTS OF PROCE VS LANDINGS

print("")
print("")
print("")
print("------------------------------------------------------------------------------")
print("STARTING SCRIPT 9")

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
# Example 9a: Scatterplot By Species - All years


fade.list <- c(TRUE,FALSE)
fade.label <- c("Fade","NoFade")




for(fade.idx in 1:length(fade.list)){


pdf(paste("OUTPUT/Examples/Scatterplot Fade/Example9a_PricevsLandings_",fade.label[fade.idx],".pdf",sep=""),width=11, height=8.5,onefile=TRUE)

# divide the plotting device into 6 panels (2x3)

layout(matrix(c(1:6),ncol=3,byrow=TRUE))

for(fish.grp in sort(unique(species.lookup[,"Fishery Group"]))){

	# specify margins (and trigger a new page)
	par(omi=c(0.5,0.5,0.5,0.5) , mai=c(1,0.7,.5,0.3))

	tmp.spec.list <- sort(species.lookup[species.lookup[,"Fishery Group"]==fish.grp,"Species"])
	for(species in tmp.spec.list){
	
			
		sub.idx <- HI_Landings_Data1[,"Species"]==species 

		scatterplot.fade(HI_Landings_Data1[sub.idx,"Sold"]/10^6,HI_Landings_Data1[sub.idx, "AdjPricePerLb"],
			x.label="Sold (Mill Lb)",y.label="$/lb (2015$)", fade=fade.list[fade.idx])
		title(species.lookup[species.lookup[,"Species"]==species,"Species Label Long"])

			
		} # end looping through species
		title(main=fish.grp,outer=TRUE,cex.main=2.5,col.main="darkblue")
		
		
	} # end looping through Fishery Group
	
dev.off()

}



