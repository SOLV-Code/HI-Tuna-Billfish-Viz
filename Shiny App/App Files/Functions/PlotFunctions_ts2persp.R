#

#############################################################################
# ts2persp() CUSTOM FUNCTION
# Created by Gottfried Pestal (Solv Consulting Ltd.)
# Version 1 - August 5, 2015
##############################################################################


# Notes: 
# designed to feed into the r2stl() function created by Ian Walker (http://drianwalker.com/), which sets up a 
# corresponding object for 3D printing
# Cannot handle missing data! (i.e every year needs a numeric value >= 0)

##########################################################################
# DEFINE THE FUNCTION

ts2persp <- function(series, series.depth=10, base=c(8,8,8,8,16), pedestal = c(1,1,1,1,0)){

# specify all dimensions in integer units. final stl file is without dimensions and can be sized as needed
# basic dimension is 4 units per year in time series. All other dim are relative to that
# series = time series values scaled to desired height in integer units
# series.depth = depth of each bar in the time series ; default is 4
# base  = vector of 5 values specifying width of borders and height of a base around the series; default = c(2,2,2,2,16)
# pedestal = vector of 5 values specifying width of borders and height of a pedestal around the base; default = c(5,15,5,15,10)


# check for missing values and cancel if any found
if(sum(is.na(series))>0){stop("Cannot plot missing values")}



# fill up a matrix, each cell corresponds to a square 1 x 1 integer units
# the series takes up  (4*length(series)) x series.dim integer units
mat.out <- matrix(0,nrow=series.depth,ncol=length(series)*4,byrow=TRUE)

srs.coord <- seq(1,dim(mat.out)[2],by=4) 

# add series values to each cell in the 4 cell block (should simplify)
for(row in 1:series.depth){
		mat.out[row,srs.coord] <- series
		mat.out[row,srs.coord+1] <- series
		mat.out[row,srs.coord+2] <- series
		mat.out[row,srs.coord+3] <- series
} # end loop



# add base height and cells around the series
base.top <- matrix(0,nrow=base[3],ncol=length(series)*4)
base.bot <- matrix(0,nrow=base[1],ncol=length(series)*4)
mat.out <- rbind(base.top, mat.out, base.bot)
base.start <- matrix(0,ncol=base[2],nrow=dim(mat.out)[1])
base.end <- matrix(0,ncol=base[4],nrow=dim(mat.out)[1])
mat.out <- cbind(base.start,mat.out,base.end)
mat.out[,] <- mat.out[,]+base[5]


# add pedestal height and cells around the series
ped.top <- matrix(0,nrow=pedestal[3],ncol=dim(mat.out)[2])
ped.bot <- matrix(0,nrow=pedestal[1],ncol=dim(mat.out)[2])
mat.out <- rbind(ped.top, mat.out, ped.bot)
ped.start <- matrix(0,ncol=pedestal[2],nrow=dim(mat.out)[1])
ped.end <- matrix(0,ncol=pedestal[4],nrow=dim(mat.out)[1])
mat.out <- cbind(ped.start,mat.out,ped.end)
mat.out[,] <- mat.out[,]+pedestal[5]

mat.out

} # end ts2persp function

