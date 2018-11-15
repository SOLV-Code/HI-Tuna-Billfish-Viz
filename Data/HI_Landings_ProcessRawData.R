#specify folder where raw data is stored
path.rawdata <- "Raw Data/HI Reported Landings/"
path.rawdata.infl <-  "Raw Data/Inflation Adjustment/"
path.procdata <- "Processed Data"

# use these during building/debugging
#current.dir <- getwd() # run first time when move to a new location
#setwd(current.dir)    # this resets if the script crashes somewhere else

# get list of files, and remove the metadata file
raw.files.list <- dir(path.rawdata)
raw.files.list  <- raw.files.list[grep("Totals.csv",raw.files.list)]
print(raw.files.list)

# start the long data inventory (~ pivot table input)
HI_Landings <- read.csv(paste(path.rawdata,raw.files.list[1],sep=""),stringsAsFactors=FALSE)
HI_Landings <- cbind(HI_Landings,Year =as.numeric(substr(raw.files.list[1],1,4)))


# loop through the remaining files and add to the inventory
for(i in 2:length(raw.files.list)){

table.tmp <- read.csv(paste(path.rawdata,raw.files.list[i],sep=""),stringsAsFactors=FALSE)
table.tmp <- cbind(table.tmp,Year =as.numeric(substr(raw.files.list[i],1,4)))
HI_Landings <- rbind(HI_Landings,table.tmp)

}

# create starting point for reorganized data set
HI_Landings.reorg <- HI_Landings
dimnames(HI_Landings.reorg)[[2]][5] <-"PricePerLb" # Should fix this to be more generic

#insert NA rows for missing years (by species)
#loop through all combinations of species and year to fill in the rows
#loops and nested loops not ideal, but fast enough in this case

species.list <- sort(unlist(unique(HI_Landings.reorg[,"Species"])))
year.list <- min(HI_Landings.reorg[,"Year"]):max(HI_Landings.reorg[,"Year"])

for(spec in species.list){
	for(yr in year.list){

	if(!(yr %in% HI_Landings.reorg[HI_Landings.reorg[,"Species"]==spec,"Year"])){
		HI_Landings.reorg <- rbind(HI_Landings.reorg,c(spec,rep(NA,dim(HI_Landings.reorg)[2]-2),yr))
		}
}}


# sort by species alphabetically and remove blanks from species labels
HI_Landings.reorg <- HI_Landings.reorg[order(HI_Landings.reorg[,"Year"],decreasing=FALSE),]
HI_Landings.reorg <- HI_Landings.reorg[order(HI_Landings.reorg[,"Species"]),]
HI_Landings.reorg[,"Species"] <- gsub(" ","",HI_Landings.reorg[,"Species"] ) 

# convert values to numeric
HI_Landings.reorg[,"Value"] <- as.numeric(gsub(",","",gsub(" ","",gsub("\\$","",HI_Landings.reorg[,"Value"] )) ))
HI_Landings.reorg[,"PricePerLb"] <- as.numeric(gsub(",","",gsub(" ","",gsub("\\$","",HI_Landings.reorg[,"PricePerLb"] )) ))
HI_Landings.reorg[,"Caught"] <- as.numeric(gsub(",","",gsub(" ","",gsub("\\$","",HI_Landings.reorg[,"Caught"] )) ))
HI_Landings.reorg[,"Sold"] <- as.numeric(gsub(",","",gsub(" ","",gsub("\\$","",HI_Landings.reorg[,"Sold"] )) ))
HI_Landings.reorg[,"Year"] <- as.numeric(HI_Landings.reorg[,"Year"])

# add inflation adjustment
US_Infl <- read.csv(paste(path.rawdata.infl,"US_InflationAdjustments.csv",sep=""),stringsAsFactors=FALSE)
HI_Landings.reorg <- cbind(HI_Landings.reorg,InflAdj=NA,AdjValue=NA,AdjPricePerLb=NA)

for(yr in year.list){HI_Landings.reorg[HI_Landings.reorg[,"Year"]==yr,"InflAdj"] <- US_Infl[US_Infl[,"Year"]==yr,"Scalar"]}
HI_Landings.reorg[,"AdjValue"] <- HI_Landings.reorg[,"Value"]*HI_Landings.reorg[,"InflAdj"]
HI_Landings.reorg[,"AdjPricePerLb"] <- HI_Landings.reorg[,"PricePerLb"]*HI_Landings.reorg[,"InflAdj"]



# Change NA to -9999 in order to get it into Access (better way?)
HI_Landings.reorg[is.na(HI_Landings.reorg)] <- -99999

write.csv(HI_Landings.reorg,file=paste(path.procdata,"Hawai_Landings_Data_Processed.csv",sep="/"),row.names=FALSE)
write.csv(US_Infl,file=paste(path.procdata,"US_Inflation_Adj_Processed.csv",sep="/"),row.names=FALSE)














