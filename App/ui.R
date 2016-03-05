library(shiny)
library(shinydashboard)

dashboardPage(skin="red",
  dashboardHeader(title='Forecasting Analytics'),
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
        menuItem("Input Data", tabName = "inputData", icon = icon("download")),
        menuItem("Model Filters", tabName = "modelFilters", icon = icon("line-chart")),
        conditionalPanel("input.sidebarmenu === 'inputData'",
            fileInput('file1', 'Choose CSV File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),'Comma'),
            radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote')
        ),
        conditionalPanel("input.sidebarmenu === 'modelFilters'",
            selectInput('xcol', 'X Variable', names(iris)),
            selectInput('ycol', 'Y Variable', names(iris),selected=names(iris)[[2]]),
            numericInput('clusters', 'Cluster count', 3,min = 1, max = 9)
            #uiOutput("choose_columns")
        )
    )
  ),
  dashboardBody(
    fluidRow(
      HTML("<div class='tabs' style='width: 100em !important;'>"),
      tabBox(
        id="tabs",
        tabPanel("Visualize",plotOutput("boxplot")),
        tabPanel("Model",plotOutput('kmeans', click="kmeans_click"))
      ),
      HTML("</div>")
    ),
    fluidRow(
      HTML("<div class='data' style='width: 100em !important;'>"),
      box(title = "View Data",
          status = "info", 
          solidHeader = TRUE, 
          collapsible = TRUE,
          dataTableOutput('mydata')
      ),
      HTML("</div>")
    )
  )
)