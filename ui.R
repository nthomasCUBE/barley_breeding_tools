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
			fileInput("file1", "Choose Targets", multiple = TRUE, accept = c(".xlsx")),
			fileInput("file2", "Choose Targets", multiple = TRUE, accept = c(".xlsx")),
			actionButton("goButton", "Analyse dataset!")
		))),
		mainPanel(
			useShinyjs(),
			plotOutput(outputId = "plot"),
			plotOutput(outputId = "plot2")
		)
	)
)