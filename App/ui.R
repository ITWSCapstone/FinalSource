library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)

dashboardPage(
  dashboardHeader(title='Forecasting Analytics'),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styling.css") #Styling from external stylesheet
    ),
    sidebarMenu(id = "sidebarmenu",
        menuItem("Input Data", tabName = "inputData", icon = icon("download")),
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
        )
    )
  ),
  dashboardBody(
    fluidRow(
      HTML("<div class='tabs' style='width: 100em !important;'>"),
      tabBox(
        id="tabs",
        tabPanel("Visualize",uiOutput("choose_column1"),
          carouselPanel(
            plotOutput("boxplot"),
            plotOutput('kmeans', click="kmeans_click"),
            auto.advance=TRUE
          )
        ),
        tabPanel("Model","Model")
      ),
      HTML("</div>")
    ),
    fluidRow(
      bsCollapsePanel("View Data",DT::dataTableOutput('mydata'))
    )
  )
)