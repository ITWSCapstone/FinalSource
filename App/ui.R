library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(ggplot2)
library(reshape2)
library(dygraphs)
library(xts)
library(party)
library(DT)
theme_set(theme_bw())
   
source("functions.R",local=TRUE)
dbHeader<-dashboardHeader(titleWidth = 350)
dbHeader$children[[2]]$children <- tags$div(tags$img(src='jjlogo.png',height='60',width='310'))
dbHeader$children[[3]]$children <- tags$div(id="buttons",actionButton("aboutLink", "About Us", icon=icon("user")), actionButton("guideLink", "Guide", icon=icon("book")))
dashboardPage(skin="red", 
  dbHeader,
  dashboardSidebar(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styling.css") #styling from external stylesheet
      ),
      sidebarMenu(id = "sidebarmenu",
        menuItem("Input Data", tabName = "inputData", icon = icon("download")),
        conditionalPanel("input.sidebarmenu === 'inputData'", id="cpanel1",
            fileInput('file1', 'Choose CSV File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),'Comma'),
            radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote')
        ),
        menuItem("Visualization Filters", tabName = "visualFilters", icon = icon("bar-chart")),
        conditionalPanel("input.sidebarmenu ==='visualFilters'",
            uiOutput("vis"),
            uiOutput("groupcol")
        )
    )
  ),
  dashboardBody(
    useShinyjs(),
    bsModal("aboutPage", "About Us", "aboutLink", size = "large", ######POP UP FOR ABOUT PAGE
    h1("About Us", align = "center", id = "About"),
    h4("About this application?", align = "left", class = "aboutques"),
    p("Our mission for this project is to create an efficient, automated and interactive interface which will allow stakeholders to upload their data in a specified format and output the appropriate forecasting method along with data visualization. 
      The application will educate end-users on forecasting techniques to help format data, choose appropriate models, and ultimately allow Johnson & Johnson to make better data-driven decisions."),
    h4("Purpose of this application", align = "left", class = "aboutques"),
    p("Johnson & Johnson guides their decision making through values spelled out in The Credo. The Credo challenges Johnson & Johnson to put the needs and well-being of the people they serve first. This project helps to serve the employees and ultimately the doctors, nurses and patients, mothers and fathers, and any others who use Johnson & Johnson’s products and services. 
      Through building this company-wide forecasting application, employees will be able to effectively analyze data to better serve the interests of Johnson & Johnson’s customers. "),
   h4("If i am having issues with this application, what should i do?", align = "left", class = "aboutques"),
    p("You are able to play around with the data until you feel comfortable to use your original data. You can follow step by step tutorials in the guide section for assitance 
      Please contact ---- for further questions"),
   img(src="jnjaslogo.png", height = 100, align = "center")
    
   
    ),
    
    bsModal("guidePage", "Guide", "guideLink", size = "large", ######POP UP FOR GUIDE PAGE
    h1("Easy Guide",align = "center", id = "Guide"),
    img(src="Step1.png", align = "left"),
    p("Upload a file"),
    img(src="Step2.png", align = "middle"),
    p("View Models and Visualization"),
    img(src="Step3.png", align = "right"),
    p("Use filters to predict")
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
            auto.advance=FALSE 
          )
        ),
        tabPanel("Model",
          carouselPanel(
            box(id="model1_box",title="K Means Clustering",width=12,solidHeader = TRUE,
              div(style="display:inline-block; padding-right:8px;",uiOutput("clust_dep")),
              div(style="display:inline-block; padding-right:8px;",uiOutput("clust_indep")),
              div(style="display:inline-block; padding-right:8px;",numericInput('clusters', 'Cluster count', 3,min = 1, max = 9)),
              plotOutput('model1',click="model1click"),
              hidden(
                tableOutput("model1_info")
              )
            ),
            box(id="model2_box",title="Bivariate Linear Regression",width=12,solidHeader = TRUE,
              div(style="display:inline-block; padding-right:8px;",uiOutput("lm_dep")),
              div(style="display:inline-block; padding-right:8px;",uiOutput("lm_indep")),
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
                div(style="display:inline-block; padding-right:8px;",uiOutput("tree_dep")),
                div(style="display:inline-block; padding-right:8px;",uiOutput("tree_indep")),
                plotOutput('model3',click="model3click")
            ),
            box(id="model4_box",title="ARIMA",width=12,solidHeader = TRUE,
                div(style="display:inline-block; padding-right:8px;",uiOutput("arima_dep")),
                div(style="display:inline-block; padding-right:8px;",uiOutput("arima_indep")),
                div(style="display:inline-block; padding-right:8px;",uiOutput("timecol")),
                plotOutput('model4',click="model4click"),
                hidden(
                  verbatimTextOutput("model4_info")
                )
            ),
            auto.advance=FALSE
          )
        )
      ),
      HTML("</div>")
    ),
    div(style="display:inline-block",selectInput("plots", "Choose Plot(s)", choices=list("Boxplot","Density Plot","Scatter Plot","K-Means","Linear Model","Decision Tree","ARIMA"), multiple=TRUE)),
    div(style="display:inline-block;",downloadButton('downloadPlots')),
    bsCollapsePanel("View Data",bsCollapsePanel("Summary",verbatimTextOutput("summary")), DT::dataTableOutput('mydata'))
  )
)