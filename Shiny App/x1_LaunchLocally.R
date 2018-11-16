

# Load the function that does the model set-up and launches the GUI
source("App Files/R/1_LaunchApp.R")

# testing the files
#sink("test.txt")
#parse("App Files/server.R")
#sink()

#sink("test.txt")
#parse("App Files/ui.R")
#sink()


# Run the function to launch locally
launchForecastR(appDir.use="App Files",fun.path="App Files/R",local=TRUE)



# go to last deployed server version
launchForecastR(local=FALSE)







