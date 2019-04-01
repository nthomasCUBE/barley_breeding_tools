library(d3heatmap)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(shinythemes)

options(stringsAsFactors=FALSE)
options(shiny.maxRequestSize = 50*1024^2)

ui <- fluidPage(  
tags$head(
	tags$style(HTML("
	.shiny-output-error {
	visibility: hidden;
}
body {
	#background-color: #23443333;
}
body, label, input, button, select { 
	font-family: 'Arial';
}"))
  ), 
  theme = shinytheme("sandstone"),  useShinyjs(), useShinyalert(), 
	sidebarLayout(
		sidebarPanel(
		tabsetPanel(id = "tabset",
		tabPanel("Genome distribution",
			fileInput("file1", "Choose Targets (e.g. 9K_*)", multiple = TRUE, accept = c(".xlsx")),
			fileInput("file2", "Choose Targets (e.g. 3_*)", multiple = TRUE, accept = c(".xlsx")),
			selectInput("select",label=h3("Select genetic Map"),choices=list("all_SNP markers"="all","MxS"="MxS","MxB Comadran"="MxB","combined"="combined"),selected=1),
			selectInput("chromosomes",label=h3("Chromosomes"),choices=list("chr1H"="chr1H","chr2H"="chr2H","chr3H"="chr3H","chr4H"="chr4H","chr5H"="chr5","chr6H"="chr6H","chr7H"="chr7H"),selected=1),
			radioButtons("shared", "Only shared marker:",c("true" = "true","false" = "false")),
			actionButton("goButton", "Analyse dataset!")),
		tabPanel("Chi-Square",
			fileInput("file4","Choose Allele-distribution (e.g. Tables_dp3__map_for Robert)",multiple = TRUE, accept = c(".csv")),
			actionButton("goButton2", "Analyse dataset!")
		))),
		mainPanel(
			useShinyjs(),
			plotOutput(outputId = "plot"),
			plotOutput(outputId = "plot2")
		)
	)
)