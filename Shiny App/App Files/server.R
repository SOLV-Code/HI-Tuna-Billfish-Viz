



function(input, output, session) {



# Think it needs these here for the server deployment
	library("ggplot2")
	library("DT")
	library("markdown")
	library("rmarkdown")
	
	library("shiny")
	library("shinydashboard")
	library("shinyjqui")
	library("shinyFiles")
	
	fn.file.list <- list.files(path="Functions",pattern=".R") # get all .R files 
	for(file.source in fn.file.list){source(paste("Functions",file.source,sep="/"))	} 

	# read in the data file
	data.use <- read.csv("Data/Hawai_Landings_DashboardInput.csv",stringsAsFactors=FALSE)

	
	data.sub.1species <- reactive({
			yrs.window <- input$yr.range.1species[1]:input$yr.range.1species[2]
			records.keep <-  data.use$Year %in% yrs.window & data.use$SpeciesLabelLong == input$species.1species 
			data.sub <- data.use[records.keep,]
				})
	
	
   
   	output$tsplot.1species <- renderPlot({
			data.file.tmp <- data.sub.1species()	
			par(mfrow=c(2,1))
			
			
			ts.plot(data.file.tmp$Year ,data.file.tmp[,input$var1.1species] ,xlim=range(data.file.tmp$Year),
					ma=4,minmaxpts=TRUE,cex.axis=1)
				title(main=input$var1.1species,cex.main=1.2,line=0)
			
			ts.plot(data.file.tmp$Year ,data.file.tmp[,input$var2.1species] ,xlim=range(data.file.tmp$Year), 
						ma=4,minmaxpts=TRUE,cex.axis=1)
				title(main=input$var2.1species,cex.main=1.2,line=0)

			title(main=input$species.1species,outer=TRUE,cex.main=1.4,line=-2,xpd=NA,col.main="darkblue")
			
		})
   
   	output$percrankplot.1species <- renderPlot({
			data.file.tmp <- data.sub.1species()	 
			par(mfrow=c(2,1))
			

		if(input$fancy.percrank.1species){
			perc.rank.plot(data.file.tmp[,input$var1.1species], ma=4,type="fancy", yrs.lab=range(data.file.tmp$Year))
			perc.rank.plot(data.file.tmp[,input$var2.1species], ma=4,type="fancy", yrs.lab=range(data.file.tmp$Year))
			}
			
		if(!input$fancy.percrank.1species){	
			perc.rank.plot(data.file.tmp[,input$var1.1species], ma=4,type="basic")
			perc.rank.plot(data.file.tmp[,input$var2.1species], ma=4,type="basic")	
			}
			
		})
   
   	output$scatterplot.1species <- renderPlot({
			data.file.tmp <- data.sub.1species()	 
			par(pty="s")
			
		scatterplot.fade(data.file.tmp[,input$var1.1species],data.file.tmp[,input$var2.1species],
			x.label=input$var1.1species,y.label=input$var2.1species, fade=input$scatterplotfade.1species)
		
		title(input$species.1species, cex.main=1.4,col.main="darkblue")
			
			
			
		})
    
   
      	output$fillerplot <- renderPlot({
			data.file.tmp <- data.sub.1species()	 
			par(mfrow=c(2,1))
			plot(1:5,1:5,main="placeholder")
		})


	# output$settings.table <- renderTable({   fc.settings() })











}

