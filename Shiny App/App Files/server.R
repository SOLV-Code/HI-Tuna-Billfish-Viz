



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
	
	
	
	data.sub.3species <- reactive({
			yrs.window <- input$yr.range.3species[1]:input$yr.range.3species[2]
			records.keep <-  data.use$Year %in% yrs.window & data.use$SpeciesLabelLong %in% c(input$species1.3species,input$species2.3species,input$species3.3species)
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
    
  


	output$tsplot.3species <- renderPlot({
			data.file.tmp <- data.sub.3species()	 

		ts1.idx <- data.file.tmp$SpeciesLabelLong == input$species1.3species
		ts2.idx <- data.file.tmp$SpeciesLabelLong == input$species2.3species	
		ts3.idx <- data.file.tmp$SpeciesLabelLong == input$species3.3species				
			
		species.lookup <- unique(data.file.tmp[,c("SpeciesLabelShort","SpeciesLabelLong","SpeciesName")])
		#print(species.lookup)	
			
			
		par(mfrow=c(2,2))
			
			
			ts.plot(data.file.tmp[ts1.idx ,"Year"] ,data.file.tmp[ts1.idx,input$var.3species] ,xlim=range(data.file.tmp$Year),
					ma=4,minmaxpts=TRUE,cex.axis=1)
				title(main=input$species1.3species,cex.main=1.2,line=0)	
			
			ts.plot(data.file.tmp[ts2.idx ,"Year"] ,data.file.tmp[ts2.idx,input$var.3species] ,xlim=range(data.file.tmp$Year),
					ma=4,minmaxpts=TRUE,cex.axis=1)
				title(main=input$species2.3species,cex.main=1.2,line=0)	
						
			ts.plot(data.file.tmp[ts3.idx ,"Year"] ,data.file.tmp[ts3.idx,input$var.3species] ,xlim=range(data.file.tmp$Year),
					ma=4,minmaxpts=TRUE,cex.axis=1)
				title(main=input$species3.3species,cex.main=1.2,line=0)	
			
			title(main=input$var.3species,cex.main=1.4,line=-1, outer= TRUE, col.main="darkblue")	
			
			
			
			# table with species info
			plot(1:20,1:20,axes=FALSE,xlab="",ylab="",type="n")
				x.vals <- c(1,5,13.5)
				y.vals <- c(18,17.5,16,14.5,13)
				text(x.vals,y.vals[1],c("Label","Name", "Species"),font=2,adj=c(0,0),cex=1.2)
				abline(h=y.vals[2])
				text(x.vals[1],y.vals[3:5],species.lookup[,"SpeciesLabelShort"],c(0,0),cex=1.2)
				text(x.vals[2],y.vals[3:5],species.lookup[,"SpeciesLabelLong"],c(0,0),cex=1.2)
				text(x.vals[3],y.vals[3:5],species.lookup[,"SpeciesName"],c(0,0),font=3,cex=1.2)
			
			
			
		})

	
	output$percrankplot.3species <- renderPlot({
			data.file.tmp <- data.sub.3species()	 

		ts1.idx <- data.file.tmp$SpeciesLabelLong == input$species1.3species
		ts2.idx <- data.file.tmp$SpeciesLabelLong == input$species2.3species	
		ts3.idx <- data.file.tmp$SpeciesLabelLong == input$species3.3species				
			
		species.lookup <- unique(data.file.tmp[,c("SpeciesLabelShort","SpeciesLabelLong","SpeciesName")])
		#print(species.lookup)	
			
			
		par(mfrow=c(2,2))
			
			
		if(input$fancy.percrank.3species){
			perc.rank.plot(data.file.tmp[ts1.idx,input$var.3species] , ma=4,type="fancy", yrs.lab=range(data.file.tmp$Year))
				title(main=input$species1.3species,cex.main=1.2,line=0)	
			perc.rank.plot(data.file.tmp[ts2.idx,input$var.3species] , ma=4,type="fancy", yrs.lab=range(data.file.tmp$Year))
				title(main=input$species2.3species,cex.main=1.2,line=0)	
			perc.rank.plot(data.file.tmp[ts3.idx,input$var.3species] , ma=4,type="fancy", yrs.lab=range(data.file.tmp$Year))
				title(main=input$species3.3species,cex.main=1.2,line=0)	
			}
			
		if(!input$fancy.percrank.3species){	
			perc.rank.plot(data.file.tmp[ts1.idx,input$var.3species] , ma=4,type="basic")
				title(main=input$species1.3species,cex.main=1.2,line=0)	
			perc.rank.plot(data.file.tmp[ts2.idx,input$var.3species] , ma=4,type="basic")
				title(main=input$species2.3species,cex.main=1.2,line=0)	
			perc.rank.plot(data.file.tmp[ts3.idx,input$var.3species] , ma=4,type="basic")
				title(main=input$species3.3species,cex.main=1.2,line=0)	
			}
			

			
			title(main=input$var.3species,cex.main=1.4,line=-1, outer= TRUE, col.main="darkblue")	
			
			
			
			# table with species info
			plot(1:20,1:20,axes=FALSE,xlab="",ylab="",type="n")
				x.vals <- c(1,5,13.5)
				y.vals <- c(18,17.5,16,14.5,13)
				text(x.vals,y.vals[1],c("Label","Name", "Species"),font=2,adj=c(0,0),cex=1.2)
				abline(h=y.vals[2])
				text(x.vals[1],y.vals[3:5],species.lookup[,"SpeciesLabelShort"],c(0,0),cex=1.2)
				text(x.vals[2],y.vals[3:5],species.lookup[,"SpeciesLabelLong"],c(0,0),cex=1.2)
				text(x.vals[3],y.vals[3:5],species.lookup[,"SpeciesName"],c(0,0),font=3,cex=1.2)
			
			
			
		})	
		
		
	
	output$compositionplot.3species <- renderPlot({
			data.file.tmp <- data.sub.3species()	 

		ts1.idx <- data.file.tmp$SpeciesLabelLong == input$species1.3species
		ts2.idx <- data.file.tmp$SpeciesLabelLong == input$species2.3species	
		ts3.idx <- data.file.tmp$SpeciesLabelLong == input$species3.3species	
		
		mat.use <- rbind(data.file.tmp[ts1.idx,input$var.3species],
						data.file.tmp[ts2.idx,input$var.3species],
						data.file.tmp[ts3.idx,input$var.3species]
						)
		
		par(mai=c(1,2,1,1))
			
			greyfish_sbplot(y.mat=mat.use,perc=TRUE, 
					fig.lab= input$var.3species, x.lab=NULL, y.lab=NULL,
					x.label.start=min(data.file.tmp$Year),
					panel.size="small", p.cex.leg=NULL,
					show.legend=TRUE)
		
				
			points(rep(par("usr")[1]-((par("usr")[2]-par("usr")[1])*0.3),3), c(0.95,1,1.05), 
						pch=22,cex=2,col=c("dodgerblue","lightgrey","darkblue"),bg=c("dodgerblue","lightgrey","darkblue"),xpd=NA)
		
			text(rep(par("usr")[1]-((par("usr")[2]-par("usr")[1])*0.3),3), c(0.95,1,1.05),
					labels=c(input$species3.3species,input$species2.3species,input$species1.3species),
						adj=-0.1,xpd=NA)
		
		
			
		})



	



  
      	output$fillerplot <- renderPlot({
			data.file.tmp <- data.sub.1species()	 
			par(mfrow=c(2,1))
			plot(1:5,1:5,main="placeholder")
		})


	# output$settings.table <- renderTable({   fc.settings() })











}

