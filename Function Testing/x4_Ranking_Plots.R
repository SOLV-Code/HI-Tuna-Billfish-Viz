# SCRIPT TO CREATE PLOTS THAT RANK SPECIES FOR DIFFERENT VARIABLES

print("")
print("")
print("")
print("------------------------------------------------------------------------------")
print("STARTING SCRIPT 4")


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
species.lookup <- species.lookup[order(species.lookup[,"Fishery Group"],species.lookup[,"Species Label Long"]),]



####################################################################################################################
# EXAMPLE 4: Changing % composition of  sold lb

# read in the latest version of all the functions
fn.list <- dir("Functions")
for(fn in fn.list){source(paste("Functions",fn,sep="/"))}

# test the summary function
factor.summary(var.use = "Sold", wt.use = "Sold", source.mat = HI_Landings_Data1)
factor.summary(var.use = "Value", wt.use = "Sold", source.mat = HI_Landings_Data1)
factor.summary(var.use = "AdjPricePerLb", wt.use = "Sold", source.mat = HI_Landings_Data1)


# test the ranking plot
par(mai=c(0.5,1.3,0.3,0.1))
ranking.plot(source.in=HI_Landings_Data1, var.tmp = "Sold", var.label = "Sold (lb)", avg.use = "wt.Avg",flag="B_Sword")

ranking.plot(source.in=HI_Landings_Data1, var.tmp = "AdjPricePerLb", var.label = "$/lb (adj to 2015 $)", avg.use = "wt.Avg")



# ----------------------------------------
# Plot 4a: create pdf handout with 5 species rankings on 1 page
# ----------------------------------------



# open the pdf file as a plotting device
# note: onefile = TRUE creates a new page in the same pdf file whenever the current device is full

spec.flag <- "B_Sword"

pdf("OUTPUT/Examples/Ranking Examples/Example_4a_SpeciesRanking_Handout.pdf",width=11, height=8.5,onefile=TRUE)

layout(matrix(1:6,ncol=3,byrow=TRUE))
par(omi=c(.2,.2,.2,.2),mai=c(0.6,.95,0.5,0.1))
var.list <-  c("Caught","Sold","AdjValue","AdjPricePerLb","PercRepDiff")
var.label.list <-  c("Caught (lb)","Sold (lb)","Value (adj to 2015$)","$/lb (adj to 2015$)","% Diff (Caught vs Sold)")

for(i in 1:length(var.list)){
	ranking.plot(source.in=HI_Landings_Data1, var.tmp = var.list[i], var.label = var.label.list[i], avg.use = "wt.Avg",flag=spec.flag)
	title(main=paste("Species Ranking (",min(HI_Landings_Data1[,"Year"]),"-",max(HI_Landings_Data1[,"Year"]),")",sep=""),outer=TRUE,line=-0.3,cex.main=1.8,col.main="darkblue")
	}


# add a table with species info
par(mai=c(.1,.1,.1,.1))
plot(1:20,1:20,axes=FALSE,xlab="",ylab="",type="n")

x.vals <- c(0.1,3.2,6.7,13)

y.vals <- seq(20,20-(dim(species.lookup)[1]+1))

text(x.vals,y.vals[1],c("Group","Label","Name", "Species"),font=2,adj=c(0,0),xpd=NA)
abline(h=y.vals[2])

text(x.vals[1],y.vals[3:length(y.vals)],species.lookup[,"Fishery Group"],c(0,0),xpd=NA)
text(x.vals[2],y.vals[3:length(y.vals)],species.lookup[,"Species Label Short"],c(0,0),xpd=NA)
text(x.vals[3],y.vals[3:length(y.vals)],species.lookup[,"Species Label Long"],c(0,0),xpd=NA)
text(x.vals[4],y.vals[3:length(y.vals)],species.lookup[,"Species Name"],c(0,0),font=3,xpd=NA)



dev.off()




# ----------------------------------------
# Plot 4b: create pdf handout with 5 species rankings on 1 page, but split by decade 
# ----------------------------------------

# read in the latest version of all the functions
fn.list <- dir("Functions")
for(fn in fn.list){source(paste("Functions",fn,sep="/"))}


decades.list <- list(Forties=1940:1949,Fifties=1950:1959,Sixties=1960:1969,
				Seventies=1970:1979,Eighties=1980:1989,Nineties=1990:1999,
				Aughts=2000:2009,Twentytens=2010:2019)

# open the pdf file as a plotting device
# note: onefile = TRUE creates a new page in the same pdf file whenever the current device is full
pdf("OUTPUT/Examples/Ranking Examples/Example_4b_SpeciesRanking_ByDecade_Handout.pdf",width=11, height=8.5,onefile=TRUE)




for(dec in names(decades.list)){



dec.data <- HI_Landings_Data1[HI_Landings_Data1[,"Year"] %in% decades.list[[dec]],]

if(dim(dec.data)[1]>0){

layout(matrix(1:6,ncol=3,byrow=TRUE))
par(omi=c(.2,.2,.2,.2),mai=c(0.6,.95,0.5,0.1))
var.list <-  c("Caught","Sold","AdjValue","AdjPricePerLb","PercRepDiff")
var.label.list <-  c("Caught (lb)","Sold (lb)","Value (adj to 2015$)","$/lb (adj to 2015$)","% Diff (Caught vs Sold)")

for(i in 1:length(var.list)){
	x.lim.tmp <- max(HI_Landings_Data1[,var.list[i]],na.rm=TRUE)
	ranking.plot(source.in=dec.data  , var.tmp = var.list[i], var.label = var.label.list[i], avg.use = "wt.Avg",x.lim.in=x.lim.tmp,flag=spec.flag)
	title(main=paste("Species Ranking (",min(dec.data[,"Year"]),"-",max(dec.data[,"Year"]),")",sep=""),outer=TRUE,line=-0.3,cex.main=1.8,col.main="darkblue")
	}


# add a table with species info
par(mai=c(.1,.1,.1,.1))
plot(1:20,1:20,axes=FALSE,xlab="",ylab="",type="n")

x.vals <- c(0.1,3.2,6.7,13)

y.vals <- seq(20,20-(dim(species.lookup)[1]+1))

text(x.vals,y.vals[1],c("Group","Label","Name", "Species"),font=2,adj=c(0,0),xpd=NA)
abline(h=y.vals[2])

text(x.vals[1],y.vals[3:length(y.vals)],species.lookup[,"Fishery Group"],c(0,0),xpd=NA)
text(x.vals[2],y.vals[3:length(y.vals)],species.lookup[,"Species Label Short"],c(0,0),xpd=NA)
text(x.vals[3],y.vals[3:length(y.vals)],species.lookup[,"Species Label Long"],c(0,0),xpd=NA)
text(x.vals[4],y.vals[3:length(y.vals)],species.lookup[,"Species Name"],c(0,0),font=3,xpd=NA)
} # end if data for this decade

} # end looping through decades
dev.off()






