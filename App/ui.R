library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)

source("carouselPanel.R",local=TRUE)
dashboardPage(skin="red",
  dashboardHeader(title='Forecasting Analytics'),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styling.css") #styling from external stylesheet
    ),
    sidebarMenu(id = "sidebarmenu",
        menuItem("Input Data", tabName = "inputData", icon = icon("download")),
        menuItem("Visualization Filters", tabName = "visualFilters", icon = icon("bar-chart")),
        menuItem("Model Filters", tabName = "modelFilters", icon = icon("line-chart")),
        menuItem("Guide", tabName = "guide", icon = icon("book")),
        conditionalPanel("input.sidebarmenu === 'inputData'",
            fileInput('file1', 'Choose CSV File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),'Comma'),
            radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote')
        ),
        conditionalPanel("input.sidebarmenu ==='modelFilters'",
            uiOutput("choose_kmeansX"),
            uiOutput("choose_kmeansY"),
            numericInput('clusters', 'Cluster count', 3,min = 1, max = 9)
        ),
        conditionalPanel("input.sidebarmenu ==='visualFilters'",
            uiOutput("choose_column1"),
            uiOutput("choose_column2")
        )
    )
  ),
  dashboardBody(
    fluidRow(
      HTML("<div class='tabs' style='width: 100em !important;'>"),
      tabBox(
        id="tabs",
        tabPanel("Visualize",
          carouselPanel(
            plotOutput("boxplot"),
            plotOutput("heatmap"),
            plotOutput("scatterplot"),
            auto.advance=FALSE #<<<<<<<<<<<<<<<<<<<<<<<<<<<<< WHAT SHOULD WE DO?? AUTO ADVANCE???
          )
        ),
        tabPanel("Model",plotOutput('kmeans', click="kmeans_click"))
      ),
      HTML("</div>")
    ),
    fluidRow(
      bsCollapsePanel("View Data",DT::dataTableOutput('mydata'))
    )
  )
)