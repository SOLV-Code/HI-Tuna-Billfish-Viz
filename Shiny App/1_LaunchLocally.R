# THIS IS THE SCRIPT THAT LAUNCHES THE GUI IN A BROWSER
# The actual GUI lives in ui.R.
# The function call using the GUI output as an input lives in server.R

# functions to load/install required packages
load_or_install <- function(package_names){  
 for(package_name in package_names){ 
                if(!is_installed(package_name)){install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")}  
 library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)  
 }  
}

is_installed <- function(mypkg){ is.element(mypkg, installed.packages()[,1])  }

# load/install functions needed for shiny dashboard
load_or_install(c("shiny","shinydashboard","shinyjqui","shiny","shinydashboard","DT","ggplot2","shinyFiles"))


# run the app
runApp(appDir = "App Files")






