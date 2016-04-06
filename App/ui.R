library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(ggplot2)
library(reshape2)
library(dygraphs)
library(xts)
library(tree)
library(knitr)
library(DT)
theme_set(theme_bw())
   
source("functions.R",local=TRUE)
dbHeader<-dashboardHeader(titleWidth = 350)
dbHeader$children[[2]]$children <-  tags$div(tags$img(src='jjlogo.png',height='60',width='310'))
dashboardPage(skin="red", 
  dbHeader,
  dashboardSidebar(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styling.css") #styling from external stylesheet
      ),
      sidebarMenu(id = "sidebarmenu",
        menuItem("Input Data", tabName = "inputData", icon = icon("download")),
        menuItem("Visualization Filters", tabName = "visualFilters", icon = icon("bar-chart")),
        conditionalPanel("input.sidebarmenu === 'inputData'",
            fileInput('file1', 'Choose CSV File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),'Comma'),
            radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote')
        ),
        conditionalPanel("input.sidebarmenu ==='visualFilters'",
            uiOutput("vis"),
            uiOutput("groupcol")
        ),
        
        actionButton("aboutLink", "About Us", icon=icon("user")),
        actionButton("guideLink", "Guide", icon=icon("book"))
        ######HTML("<div class='aboutLink'>About Us</div>")
    )
  ),
  dashboardBody(
    useShinyjs(),
    bsModal("aboutPage", "About Us", "aboutLink", size = "large", ######POP UP FOR ABOUT PAGE
            
            tags$head(
              tags$style(HTML("
                              .modal{
                              
                              }
                              .modal-body {
                              
                              }
                              "))
              ),
            
            HTML("<h1>About Us</h1>")
            
            
            
            ),
    bsModal("guidePage", "Guide", "guideLink", size = "large", ######POP UP FOR GUIDE PAGE
            
            tags$head(
              tags$style(HTML("
                              .modal{
                              
                              }
                              .modal-body {
                              
                              }
                              "))
              ),
            
            
            h1("Guide")
            
            
            ),
    fluidRow(
      HTML("<div class='tabs'>"),
      tabBox(
        id="tabs", width=12,
        tabPanel("Visualize",
          carouselPanel(
            box(title="Boxplot", width=12,solidHeader=TRUE,plotOutput("boxplot")),
            box(title="Densityplot",width=12,solidHeader=TRUE,plotOutput("densityplot",dblclick = "plotdblclick",brush = brushOpts(id = "brush",resetOnNew = TRUE))),
            box(title="Scatterplot",width=12,solidHeader=TRUE,plotOutput("scatterplot")),
            auto.advance=FALSE #<<<<<<<<<<<<<<<<<<<<<<<<<<<<< WHAT SHOULD WE DO?? AUTO ADVANCE???
          )
        ),
        tabPanel("Model",
          carouselPanel(
            box(id="model1_box",title="K Means Clustering",width=12,solidHeader = TRUE,
              uiOutput("clust_dep"),
              uiOutput("clust_indep"),
              numericInput('clusters', 'Cluster count', 3,min = 1, max = 9),
              plotOutput('model1',click="model1click"),
              hidden(
                verbatimTextOutput("model1_info")
              )
            ),
            box(id="model2_box",title="Linear Regression",width=12,solidHeader = TRUE,
              uiOutput("lm_dep"),
              uiOutput("lm_indep"),
              plotOutput('model2',click="model2click"),
              hidden(
                div(id="model2_i",
                  box( title="Summary",
                    verbatimTextOutput("model2_info")   
                  ),
                  box( title="Residuals",
                    plotOutput("model2_resid") 
                  )
                )
              )
            ),
            box(id="model3_box",title="Decision Tree",width=12,solidHeader = TRUE,
                uiOutput("tree_dep"),
                uiOutput("tree_indep"),
                plotOutput('model3',click="model3click"),
                hidden(
                  verbatimTextOutput("model3_info")
                )
            ),
            box(id="model4_box",title="ARIMA",width=12,solidHeader = TRUE,
                uiOutput("arima_dep"),
                uiOutput("arima_indep"),
                uiOutput("timecol"),
                plotOutput('model4',click="model4click"),
                hidden(
                  verbatimTextOutput("model4_info"),
                  plotOutput("model4_resid")
                )
            ),
            auto.advance=FALSE #<<<<<<<<<<<<<<<<<<<<<<<<<<<<< WHAT SHOULD WE DO?? AUTO ADVANCE???
          )
        )
      ),
      HTML("</div>")
    ),
    fluidRow(
      downloadButton('downloadPlots'),
      bsCollapsePanel("View Data",bsCollapsePanel("Summary",verbatimTextOutput("summary")), DT::dataTableOutput('mydata'))
    )
  )
)