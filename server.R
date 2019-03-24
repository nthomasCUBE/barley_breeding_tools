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
	v <- reactiveValues(species=NULL, sumo=NULL, d1=NULL, d2s1=NULL, d2s2=NULL, d2s3=NULL)

	observeEvent(input$goButton,{
		source("methods.R")
		v$file1=input$file1$datapath
		v$file2=input$file2$datapath
		v$my_sel=input$select
		v$my_chr=input$chromosomes
		output$plot=renderPlot({
			my_plot=gene_distribution(v$file1,v$file2,v$my_sel,v$my_chr,v)
			#print(my_plot)
		})
	})
}
