# ----------------------------------------------------------------------
# NEW VERSION
# just read in the processed files
# ----------------------------------------------------------------------
# using the version saved in the Shiny Appp folder

HI_Landings_Data1 <- read.csv("../Shiny App/App Files/Data/Hawai_Landings_DashboardInput.csv",stringsAsFactors=FALSE)
US_Infl <- read.csv("../Shiny App/App Files/Data/US_Inflation_Adj_Processed.csv",stringsAsFactors=FALSE)



# ----------------------------------------------------------------------
# OLD VERSION
# Now have code to bypass the Acess database
# ----------------------------------------------------------------------


if(FALSE){

# Script to extract data from Access database

print("")
print("")
print("")
print("------------------------------------------------------------------------------")
print("STARTING SCRIPT 1")

# RODBC package needed to link to Access DB. Check if installed, if not, install it.
# RODBC only runs on 32bit R, so need to check that first.
if(Sys.getenv("R_ARCH")=="/i386"){print("Running 32bit R. Can use RODBC")}
if(Sys.getenv("R_ARCH")=="/x64"){warning("Running 64bit R. RODBC only available for 32bit R!");stop()}
if(!("RODBC" %in% rownames(installed.packages()))){install.packages("RODBC")}

# load RODBC and get the data table
library(RODBC)

# save current directory, then change to database directory
current.dir <- getwd(); setwd("../2 Database")

db.conn <- odbcConnectAccess("HI_Landings_Database.mdb")
HI_Landings_Data1 <- sqlFetch(db.conn,"DashboardInput1",stringsAsFactors=FALSE)
US_Infl <- sqlFetch(db.conn,"US_Inflation_Adj_Processed",stringsAsFactors=FALSE)
close(db.conn)

setwd(current.dir)


# convert -9999 into NA  (Should find a better way to handle NA in transition from R-> Access -> R)
HI_Landings_Data1[HI_Landings_Data1 < -9998] <- NA

# check the data
print(HI_Landings_Data1)

} # end bypassing old version



