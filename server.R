library(d3heatmap)
library(officer)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(shinythemes)

options(stringsAsFactors=FALSE)

server <- function(input, output, session)
{
	v <- reactiveValues(species=NULL, sumo=NULL)

	observeEvent(input$goButton,{
		source("methods.R")
		v$file1=input$file1$datapath
		v$file2=input$file2$datapath
		output$plot=renderPlot({
			my_plot=gene_distribution(v$file1,v$file2)
			print(my_plot)
		})
	})
}
