# SCRIPT TO CREATE PLOTS THAT SHOW CHANGING COMPOSITION OF LANDINGS OVER TIME

print("")
print("")
print("")
print("------------------------------------------------------------------------------")
print("STARTING SCRIPT 3")

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

# extract data for 3 species of genus Thunnus ("True Tunas"), then reorganize into a matrix for each variable
real.tuna.list <- sort(species.lookup[species.lookup[,"Genus"]=="Thunnus"& !is.na(species.lookup[,"Genus"]),"SpeciesLabelShort"])
real.tuna.data <- HI_Landings_Data1[ HI_Landings_Data1[,"SpeciesLabelShort"] %in% real.tuna.list,]

# caught
real.tuna.mat.caught  <- rbind(real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[1],"Caught"],
					real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[2],"Caught"],
					real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[3],"Caught"])
dimnames(real.tuna.mat.caught) <- list(real.tuna.list,data.yrs) 
print(real.tuna.mat.caught)

# sold
real.tuna.mat.sold  <- rbind(real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[1],"Sold"],
					real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[2],"Sold"],
					real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[3],"Sold"])
dimnames(real.tuna.mat.sold) <- list(real.tuna.list,data.yrs) 
print(real.tuna.mat.sold)


# Value
real.tuna.mat.value  <- rbind(real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[1],"Value"],
					real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[2],"Value"],
					real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[3],"Value"])
dimnames(real.tuna.mat.value) <- list(real.tuna.list,data.yrs) 
print(real.tuna.mat.value)


# Adjusted Value
real.tuna.mat.adjvalue  <- rbind(real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[1],"AdjValue"],
					real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[2],"AdjValue"],
					real.tuna.data[real.tuna.data[,"SpeciesLabelShort"]== real.tuna.list[3],"AdjValue"])
dimnames(real.tuna.mat.adjvalue) <- list(real.tuna.list,data.yrs) 
print(real.tuna.mat.adjvalue)





####################################################################################################################
# EXAMPLE 3: Changing % composition of  sold lb

# test the % composition plot
par(mai=c(.6,.6,.6,1.2))
greyfish_sbplot(y.mat=real.tuna.mat.caught,perc=TRUE, 
		fig.lab= "Caught (lb)", x.lab=NULL, y.lab=NULL,
		x.label.start=min(data.yrs),
		panel.size=NULL, p.cex.leg=NULL)




# ----------------------------------------
# Plot 3a:  1 page plot showing changing composition over time for 3 Thunnus species, 4 different variables (3 different file formats)
# ----------------------------------------

file.type.list <- c("pdf","jpeg","eps")


if(!dir.exists("OUTPUT/Examples")){dir.create("OUTPUT/Examples")}
if(!dir.exists("OUTPUT/Examples/Changing Composition Examples")){dir.create("OUTPUT/Examples/Changing Composition Examples")}



for(file.type in file.type.list){  # loop through file types

# open the  file as a plotting device

if(file.type=="jpeg"){jpeg("OUTPUT/Examples/Changing Composition Examples/Example_3a_ChangingComp_Thunnus_Handout.jpeg",width=1100, height=850,quality=100,pointsize=18)}
if(file.type=="pdf"){pdf("OUTPUT/Examples/Changing Composition Examples/Example_3a_ChangingComp_Thunnus_Handout.pdf",width=11, height=8.5)}
if(file.type=="eps"){postscript("OUTPUT/Examples/Changing Composition Examples/Example_3a_ChangingComp_Thunnus_Handout.eps",width=11, height=8.5)}

# specify margins 
par(omi=c(0.5,0.5,0.5,0.5) , mai=c(0.6,0.6,0.6,1.2))

# divide the plotting device into 3 panels 
layout(matrix(c(1:4),ncol=2,byrow=TRUE))


greyfish_sbplot(y.mat=real.tuna.mat.caught,perc=TRUE, 
		fig.lab= "Caught (lb)", x.lab=NULL, y.lab=NULL,
		x.label.start=min(data.yrs),
		panel.size=NULL, p.cex.leg=NULL,
		show.legend=TRUE)

greyfish_sbplot(y.mat=real.tuna.mat.sold,perc=TRUE, 
		fig.lab= "Sold (lb)", x.lab=NULL, y.lab=NULL,
		x.label.start=min(data.yrs),
		panel.size=NULL, p.cex.leg=NULL,
		show.legend=TRUE)

greyfish_sbplot(y.mat=real.tuna.mat.adjvalue,perc=TRUE, 
		fig.lab= "Value (Adj to 2015 $)", x.lab=NULL, y.lab=NULL,
		x.label.start=min(data.yrs),
		panel.size=NULL, p.cex.leg=NULL,
		show.legend=TRUE)

title(main= "Changing Composition of Tuna Landings and Value",col.main="darkblue",cex.main=1.4,outer=TRUE) 

# add a table with species info

plot(1:20,1:20,axes=FALSE,xlab="",ylab="",type="n")
labels.idx <- species.lookup[,"SpeciesLabelShort"] %in% real.tuna.list
x.vals <- c(1,5.5,12)
y.vals <- c(18,17.5,16,14.5,13)
text(x.vals,y.vals[1],c("Label","Name", "Species"),font=2,adj=c(0,0))
abline(h=y.vals[2])
text(x.vals[1],y.vals[3:5],species.lookup[labels.idx,"SpeciesLabelShort"],c(0,0))
text(x.vals[2],y.vals[3:5],species.lookup[labels.idx,"SpeciesLabelLong"],c(0,0))
text(x.vals[3],y.vals[3:5],species.lookup[labels.idx,"SpeciesName"],c(0,0),font=3)


dev.off()

}


