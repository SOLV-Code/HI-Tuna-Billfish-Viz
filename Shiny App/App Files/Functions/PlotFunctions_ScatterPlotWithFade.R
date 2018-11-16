###################################################
# SCATTER PLOT WITH FADE OPTION CUSTOM PLOT FUNCTION
# Created by Gottfried Pestal (Solv Consulting Ltd.)
# Version 1 - September 3, 2012
###################################################


scatterplot.fade <- function(x,y,x.label,y.label,fade=TRUE){

if(fade==TRUE){
bg.fade <- c(rep(0,max(0,length(x)-35)),seq(0.045,1, length.out=min(35,length(x))))
bg.col <- rgb(1, 0, 0,bg.fade)
}
if(fade!=TRUE){bg.col<- "lightgrey"}

plot(x,y, pch=21, col="darkblue",bg= bg.col  ,cex=1.8,bty="n",type="p", xlab=x.label,ylab=y.label)


}
