
# Think it needs these here for the server deployment
	library("ggplot2")
	library("DT")
	library("markdown")
	library("rmarkdown")
	
	library("shiny")
	library("shinydashboard")
	library("shinyjqui")
	library("shinyFiles")



data.use <- read.csv("Data/Hawai_Landings_DashboardInput.csv",stringsAsFactors=FALSE)
species.list <- sort(unique(data.use$SpeciesLabelLong))
var.list <- c("Caught","Sold","ReportingDiff","PercRepDiff","Value","PricePerLb","InflAdj","AdjValue","AdjPricePerLb")

	

				

				
navbarPage("HI Tuna & Billfish DataViz", id = "MainTab",


	 tabPanel("Disclaimer",

fluidPage(

  titlePanel("Disclaimer"),

  fluidRow(
    column(8,
	  includeMarkdown("Markdown/disclaimer.md")
    )
  )
)


	
	  ),  # end Disclaimer tab panel


   

#################### Explore 1 Species ######################################	

  tabPanel("Explore 1 Species",
  
  
	pageWithSidebar(
	headerPanel("Explore 1 Species"),
    
	sidebarPanel(width=2,
		selectizeInput("species.1species", "Species", choices = species.list, selected = species.list[1]),
		selectizeInput("var1.1species", "Variable 1", choices = var.list, selected = "Caught"),
		selectizeInput("var2.1species", "Variable 2", choices = var.list, selected = "PricePerLb"),
		sliderInput("yr.range.1species", "Years", sep="",min = 1948, max = 2020, value = c(1948,2020)),
		checkboxInput("fancy.percrank.1species", "Fancy Percent Rank", TRUE)
		
					  
		) # end sidebar
  ,
   

     mainPanel(			
	     
	 
		 tabsetPanel(type = "tabs",
                  tabPanel("Time Series", plotOutput("tsplot.1species",width = "80%", height = "600px")),
                  tabPanel("Percent Rank",plotOutput("percrankplot.1species",width = "80%", height = "600px") ),
                  tabPanel("Scatterplot",plotOutput("scatterplot.1species",width = "80%", height = "600px") )
				  )
	 
		
	   
		) # end main panel
  
		) #end page with side bar

),


#################### Compare 3 Species ######################################	

  tabPanel("Compare 3 Species",
  
  
	pageWithSidebar(
	headerPanel("Compare 3 Species"),
    
	sidebarPanel(width=2,
		selectizeInput("species1.3species", "Species 1", choices = species.list, selected = "Albacore Tuna"),
		selectizeInput("species2.3species", "Species 2 ", choices = species.list, selected = "Bigeye Tuna"),
		selectizeInput("species3.3species", "Species 3", choices = species.list, selected = "Yellowfin Tuna"),		
		selectizeInput("var.3species", "Variable", choices = var.list, selected = "Caught"),
		sliderInput("yr.range.1species", "Years", sep="",min = 1948, max = 2020, value = c(1948,2020))
	) # end sidebar
  ,
   

     mainPanel(			
	     
	 
		 tabsetPanel(type = "tabs",
                 tabPanel("Time Series",  plotOutput("fillerplot",width = "80%", height = "600px")),
				 tabPanel("Composition", plotOutput("fillerplot",width = "80%", height = "600px")),
				 tabPanel("Composition", plotOutput("fillerplot",width = "80%", height = "600px"))
				  )
	 
		
	   
		) # end main panel
  
		) #end page with side bar

),

##################################	
	
	

#################### Overview of All Species ######################################	

  tabPanel("Overview of All Species",
  
  
	pageWithSidebar(
	headerPanel("Overview of All Species"),
    
	sidebarPanel(width=2,
		selectizeInput("var1.allspecies", "Variable 1", choices = var.list, selected = "Caught"),
		selectizeInput("var2.allspecies", "Variable 2", choices = var.list, selected = "PricePerLb"),
		sliderInput("yr.range.allspecies", "Years", sep="",min = 1948, max = 2020, value = c(1948,2020)),
		selectizeInput("species1.allspecies", "Species", choices = species.list, selected = species.list[1])
		
					  
		) # end sidebar
  ,
   

     mainPanel(			
	     
	 
		 tabsetPanel(type = "tabs",
                  tabPanel("Ranking", plotOutput("fillerplot",width = "100%", height = "600px")),
                  tabPanel("Stacked Bar", plotOutput("fillerplot",width = "100%", height = "600px") )
                   )
	 
		
	   
		) # end main panel
  
		) #end page with side bar

),

########################	
	
	 tabPanel("Help",  value= "help.panel",

fluidPage(

  titlePanel("Help Page"),

  fluidRow(
    column(8,
	  includeMarkdown("Markdown/help.md")
    )
  )
)


	
	  ),  # end Help tab panel

	  
##############################################
	  
	tabPanel("About",
	
fluidPage(

  titlePanel("About HI Tuna & Billfish DataViz"),

  fluidRow(
    column(8,
      includeMarkdown("Markdown/about.md")
    )
  )	
)	
	  )  # end about tab panel
	
	
	
) # end navbar Page


