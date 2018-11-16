# SCRIPT TO CREATE SURFACE PLOTS OF TIME SERIES AND CONVERT THEM TO SHAPEFILES FOR 3D PRINTING

print("")
print("")
print("")
print("------------------------------------------------------------------------------")
print("STARTING SCRIPT 6")

# read in latest version of the data
# commented out, because only need to run if any database changes
# NOTE: this step requires 32-bit R (due to RODBC package)
# source("1_GetData_Script.R")

# check the data
#print(HI_Landings_Data1)
# print(US_Infl)

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
# EXAMPLE 5: SURFACE PLOTS FROM A TIME SERIES




# ----------------------------------------
# Plot 5a: US INFLATION ADJUSTMENTS (1913-2015)
# ----------------------------------------

# check the time series
plot(US_Infl,type="l")


#rescale to a max height of some specified units relative to the 4 grid cells/year
US_Infl_AllYrs <- US_Infl[,2] /max(US_Infl[,2])*80

# convert time series to surface coordinates
US_Infl_AllYrs_persp <- ts2persp(US_Infl_AllYrs)

# plot surface
persp(x=1:dim(US_Infl_AllYrs_persp)[1],y=1:dim(US_Infl_AllYrs_persp)[2],z=US_Infl_AllYrs_persp,theta=45,phi=35, 
		zlim=c(0,max(US_Infl_AllYrs_persp)), scale=FALSE)

# scale to determine size of eventual object (2 = half of default size)  
scalar <- 1

# create stl file
r2stl.mod(x=c(1:dim(US_Infl_AllYrs_persp)[1])/scalar,y=c(1:dim(US_Infl_AllYrs_persp)[2])/scalar,
		z=US_Infl_AllYrs_persp/scalar,
		filename="OUTPUT/3D Print Sources/US Inflation/US_Infl_Index_1913to2015_FullSize.stl",show.persp=TRUE,z.expand=TRUE)


# scale to determine size of eventual object (2 = half of default size)  
scalar <- 2

# create stl file
r2stl.mod(x=c(1:dim(US_Infl_AllYrs_persp)[1])/scalar,y=c(1:dim(US_Infl_AllYrs_persp)[2])/scalar,
		z=US_Infl_AllYrs_persp/scalar,
		filename="OUTPUT/3D Print Sources/US Inflation/US_Infl_Index_1913to2015_HalfSize.stl",show.persp=TRUE,z.expand=TRUE)



# ----------------------------------------
# Plot 5b: US INFLATION ADJUSTMENTS (1913-2015) BUT WITH DECADE NOTCHES
# ----------------------------------------
# same as previous, except add in a 0 step before each decade

notches <- matrix(c(seq(1920,2010,by=10)-0.5,rep(0,10)),ncol=2,dimnames=list(NULL,c("Year","Scalar")))

US_Infl_AllYrs_Notch <- rbind(US_Infl,notches)
US_Infl_AllYrs_Notch <- US_Infl_AllYrs_Notch[order(US_Infl_AllYrs_Notch[,"Year"]),]
US_Infl_AllYrs_Notch <- US_Infl_AllYrs_Notch[,2] /max(US_Infl_AllYrs_Notch[,2])*80

US_Infl_AllYrs_Notch_persp <- ts2persp(US_Infl_AllYrs_Notch)
scalar <- 1
r2stl.mod(x=c(1:dim(US_Infl_AllYrs_Notch_persp)[1])/scalar,y=c(1:dim(US_Infl_AllYrs_Notch_persp)[2])/scalar,
		z=US_Infl_AllYrs_Notch_persp/scalar,
		filename="OUTPUT/3D Print Sources/US Inflation/US_Infl_Index_1913to2015_FullSize_Notch.stl",show.persp=TRUE,z.expand=TRUE)
scalar <- 2
r2stl.mod(x=c(1:dim(US_Infl_AllYrs_Notch_persp)[1])/scalar,y=c(1:dim(US_Infl_AllYrs_Notch_persp)[2])/scalar,
		z=US_Infl_AllYrs_Notch_persp/scalar,
		filename="OUTPUT/3D Print Sources/US Inflation/US_Infl_Index_1913to2015_HalfSize_Notch.stl",show.persp=TRUE,z.expand=TRUE)








# ----------------------------------------
# Plot 5c: HAWAII FISHERY LANDINGS (REPORTED SALES 
# ----------------------------------------
# same as previous, except add in a 0 step before each decade


spec.list <- unique(HI_Landings_Data1[,"Species Label Short"])


for(spec in spec.list){
print(spec)
ts.tmp <- HI_Landings_Data1[HI_Landings_Data1[,"Species Label Short"]==spec,"Sold"]
ts.tmp[is.na(ts.tmp)] <- 0 # replace NA with 0 (no bar height in 3D plot)
ts.tmp <- ts.tmp/max(ts.tmp)*80
tspersp.tmp <- ts2persp(ts.tmp)


scalar <- 1
r2stl.mod(x=c(1:dim(tspersp.tmp)[1])/scalar,y=c(1:dim(tspersp.tmp)[2])/scalar,
		z=tspersp.tmp/scalar,
		filename=paste("OUTPUT/3D Print Sources/Hawaii Landings/",spec,"_Full.stl",sep=""),
		show.persp=TRUE,z.expand=TRUE)
}


























