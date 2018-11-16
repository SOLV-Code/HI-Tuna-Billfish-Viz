# this script adds some additonal info and filters out the species not being used for the dashboard

#specify folder where processed data is stored and read in the files

path.procdata <- "Processed Data"
path.lookup <- "Lookup Files"
path.appdata <- "../Shiny App/Data"   # where to put the output so the app can use it 

HI_Landings.reorg <- read.csv(file=paste(path.procdata,"Hawai_Landings_Data_Processed.csv",sep="/"),stringsAsFactors=FALSE)
US_Infl <- read.csv(file=paste(path.procdata,"US_Inflation_Adj_Processed.csv",sep="/"),stringsAsFactors=FALSE)
species.info <- read.csv(file=paste(path.lookup,"Species_Info_Lookup.csv",sep="/"),stringsAsFactors=FALSE)


# start building the pieces for the dashboard input

dashboard.in  <- HI_Landings.reorg[,c("Species","Year","Caught","Sold")]
dashboard.in  <- cbind(dashboard.in,ReportingDiff = dashboard.in$Caught - dashboard.in$Sold)
dashboard.in  <- cbind(dashboard.in,PercRepDiff = dashboard.in$ReportingDiff / dashboard.in$Sold *100)					
dashboard.in  <- cbind(dashboard.in, HI_Landings.reorg[,c("Value" ,"PricePerLb" , "InflAdj","AdjValue","AdjPricePerLb")])

lookup.idx <- match(dashboard.in$Species , species.info$Species)

# drop all species in the landings file that are not listed in the lookup file
drop.idx <- is.na(lookup.idx)
lookup.idx <- lookup.idx[!drop.idx]
dashboard.in <- dashboard.in[!drop.idx,]

dashboard.in <- cbind(dashboard.in, species.info[lookup.idx,])


head(dashboard.in)


# put it in the processed folder
write.csv(dashboard.in,file=paste(path.procdata,"Hawai_Landings_DashboardInput.csv",sep="/"),row.names=FALSE)

# put it into the app folder
write.csv(dashboard.in,file=paste(path.appdata,"Hawai_Landings_DashboardInput.csv",sep="/"),row.names=FALSE)
write.csv(US_Infl, file=paste(path.appdata,"US_Inflation_Adj_Processed.csv",sep="/"),row.names=FALSE)









