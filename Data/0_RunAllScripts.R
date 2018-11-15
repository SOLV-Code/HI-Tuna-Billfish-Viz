# Script to run all the other scripts at once

script.list <- list.files(pattern=".R") # get all .R files 
script.list <- script.list[!(script.list %in% "0_RunAllScripts.R")] # remove this script

for(script in script.list){source(script)}










