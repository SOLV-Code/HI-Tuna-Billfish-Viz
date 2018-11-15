###################################################
# STACKED BAR CONMPARISON CUSTOM PLOT FUNCTION
# Created by Gottfried Pestal (Solv Consulting Ltd.)
# Version 2 - November 2016
###################################################


# custom function to plot stacked bar comparison

stacked.bar.comp.plot <- function(x,var.larger="Target",var.smaller="Actual",title.in="TITLE",x.lim=NULL){
# x is an array with
# 1 row for each unit (e.g. fish stock, fishery etc)
# row dimnames become barplot labels
# 2 columns with dimnames = var.larger , var.smaller
# x.lim sets range for x axis. Default = NULL, so that R uses automatic axis scaling
 
barplot(x[,var.larger],density=10,col="red",horiz=TRUE,las=1,xlim=x.lim)
barplot(x[,var.smaller],col="dodgerblue",horiz=TRUE,add=TRUE,axes=FALSE,axisnames=FALSE,las=1)
title(main=title.in)
}
