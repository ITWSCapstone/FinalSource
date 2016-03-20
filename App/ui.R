library(shiny)
library(shinydashboard)
library(shinyBS)
library(ggplot2)
theme_set(theme_bw())
library(DT)
   
source("functions.R",local=TRUE)
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
            uiOutput("cols2"),
            numericInput('clusters', 'Cluster count', 3,min = 1, max = 9)
        ),
        conditionalPanel("input.sidebarmenu ==='visualFilters'",
            uiOutput("cols1"),
            uiOutput("groupcol"),
            uiOutput("timecol")
        )
    )
  ),
  dashboardBody(
    fluidRow(
      HTML("<div class='tabs'>"),
      tabBox(
        id="tabs", width=12,
        tabPanel("Visualize",
          carouselPanel(
            plotOutput("boxplot"),
            plotOutput("densityplot"),
            plotOutput("scatterplot"),
            auto.advance=FALSE #<<<<<<<<<<<<<<<<<<<<<<<<<<<<< WHAT SHOULD WE DO?? AUTO ADVANCE???
          )
        ),
        tabPanel("Model",
          carouselPanel(
            plotOutput('model1'),
            plotOutput('model2',dblclick = "plotdblclick",brush = brushOpts(id = "brush",resetOnNew = TRUE)),
            auto.advance=FALSE #<<<<<<<<<<<<<<<<<<<<<<<<<<<<< WHAT SHOULD WE DO?? AUTO ADVANCE???
          )
        )
      ),
      HTML("</div>")
    ),
    fluidRow(
      bsCollapsePanel("View Data",verbatimTextOutput("summary"), DT::dataTableOutput('mydata'))
    )
  )
)