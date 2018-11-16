# Script to run all the other scripts at once
# This extracts the latest info from the database and creates all the plot variations
# Note: it may take a while


script.list <- list.files(pattern=".R") # get all .R files 
script.list <- script.list[!(script.list %in% "10_RunAllScripts.R")] # remove this script

for(script in script.list){source(script)}










