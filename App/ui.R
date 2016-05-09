library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(ggplot2)
library(reshape2)
library(dygraphs)
library(xts)
library(partykit)
library(Formula)
library(DT)
theme_set(theme_bw())
   
source("functions.R",local=TRUE)
dbHeader<-dashboardHeader(titleWidth = 350)
dbHeader$children[[2]]$children <- div(tags$img(src='jjlogo.png',height='60',width='310'))

dashboardPage(skin="red", 
  dbHeader,
  dashboardSidebar(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styling.css") #styling from external stylesheet
      ),
      div(id="buttons",actionButton("aboutLink", "About Us", icon=icon("user")), actionButton("guideLink", "Guide", icon=icon("book"))),
      sidebarMenu(id = "sidebarmenu",
        menuItem("Input Data", icon = icon("download"),
        menuSubItem(icon=NULL,fileInput('file1', 'Choose CSV File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
        menuSubItem(icon=NULL,checkboxInput('header', 'Header', TRUE)),
        menuSubItem(icon=NULL,radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),'Comma')),
        menuSubItem(icon=NULL,radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote'))
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
    bsModal("aboutPage", title=NULL, "aboutLink", size = "large", ######POP UP FOR ABOUT PAGE
    h1("About Us", align = "center", id = "About"),
    h4("About Forecasting Analytics", align = "left", class = "aboutques"),
    p("Our mission for this project is to create an efficient, automated and interactive interface which will allow stakeholders to upload their data in a specified format and output the appropriate forecasting method along with data visualization. 
      The application will educate end-users on forecasting techniques to help format data, choose appropriate models, and ultimately allow Johnson & Johnson to make better data-driven decisions."),
    h4("Purpose of Forecasting Analytics", align = "left", class = "aboutques"),
    p("Johnson & Johnson guides their decision making through values spelled out in The Credo. The Credo challenges Johnson & Johnson to put the needs and well-being of the people they serve first. This project helps to serve the employees and ultimately the doctors, nurses and patients, mothers and fathers, and any others who use Johnson & Johnson’s products and services. 
      Through building this company-wide forecasting application, employees will be able to effectively analyze data to better serve the interests of Johnson & Johnson’s customers. "),
    h4("First-time Users", align = "left", class = "aboutques"),
    p("You are able to play around with the data until you feel comfortable to use your original data. You can follow step by step tutorials in the guide section for assitance.
      Please contact ---- for further questions"),
   img(src="jnjaslogo.png", height = 100, align = "center")
    ),
    bsModal("guidePage", title=NULL, "guideLink", size = "large", ######POP UP FOR GUIDE PAGE
      h1("Step-by-Step Guide",align = "center", id = "Guide"),
      fluidRow(
        div(id="steps",align="center",
          HTML('<figure>'),img(id="s1",src="Step1.png"),
          img(id="s1gif",src="./Step1.gif"),
          HTML('<figcaption>Input Data</figcaption></figure><figure>'),img(id="s2", src="Step2.png"),
          img(id="s2gif",src="./step2b.gif"),
          HTML('<figcaption>Model and Visualize Data</figcaption></figure><figure>'),img(id="s3",src="Step3.png"),
          img(id="s3gif",src="./Step3.gif"),
          HTML('<figcaption>Download Results</figcaption></figure><figure>')
        )
      )
    ),
    fluidRow(
      bsTooltip("model1_info" , "Cluster means are the centroids of each cluster. The sum of squares by cluster is a measure of the total variance in the dataset because k-means minimises the spread of the samples, the sum of squares. ", placement = "left", trigger = "hover"),
      
      bsTooltip("model2_i1" , "If the p-value is less than 0.05, we reject the null hypothesis.The R-squared value is the percentage of the response variable variation that is explained by a linear model (R-squared > 0.5 is ideal). ", placement = "top", trigger = "hover"),
      
      bsTooltip("model2_i2" , "If the points are randomly dispersed around the red line, a linear regression model is appropriate for the data; otherwise, a non-linear model is more appropriate.", placement = "right", trigger = "hover"),

      bsTooltip("model3_info" , "Err represents the error sum of squares(SSE)-it is used to measure the variation within each node.", placement = "left", trigger = "hover"),

      bsTooltip("boxplot-info", "A standardized way of displaying the distribution of data based on the minimum, first quartile, median, third quartile, and maximum; the first to third quartile is the interquartile range (IQR) in which outliers = 3xIQR above the third quartile or 3xIQR below the first quartile", placement = "bottom", trigger = "hover"),
      
      bsTooltip("densityplot-info", "A curve plotted over a histogram; the histogram bars represent the frequency of occurrence by classes of data and the integral of the curve then gives the probability of any distinct value", placement = "bottom", trigger = "hover"),
      
      bsTooltip("scatterplot-info", "A plot that uses Cartesian coordinates to display values for two or more variables for a set of data; can suggest various kinds of correlations between variables", placement = "bottom", trigger = "hover"),
      
      bsTooltip("kmeans-info", "A process of partitioning a group of data points into a small number of clusters; easier to view products and sales in categories", placement = "bottom", trigger = "hover", options = NULL), 
      
      bsTooltip("bivariate-info", "An analysis of two variables for the purpose of determining the empirical relationship between them; can help determine to what extent it becomes easier to know and predict a value for one variable (a dependent variable) if we know the value of the other variable (an independent variable)", placement = "bottom", trigger = "hover", options = NULL),
      
      bsTooltip("decisiontree-info", "An analysis diagram which can help aid decision makers, when deciding between different options by projecting possible outcomes; gives the decision maker an overview of the multiple stages that will follow each possible decision", placement = "bottom", trigger = "hover", options = NULL),
      
      bsTooltip("arima-info", "An autoregressive integrated moving average that uses time series data to predict future trends; can take into account trends, seasonality, cycles, errors and non-stationary aspects of a data set when making forecasts", placement = "bottom", trigger = "hover", options = NULL),
      HTML("<div class='tabs'>"),
      tabBox(id="tabs", width=12,
        tabPanel("Visualize",
          carouselPanel(
            box(title="Boxplot", div(class="info-icon", id="boxplot-info", icon("info-circle")), width=12,solidHeader=TRUE, plotOutput("boxplot")),
            box(title="Densityplot", div(class="info-icon", id="densityplot-info", icon("info-circle")), width=12,solidHeader=TRUE,plotOutput("densityplot",dblclick = "plotdblclick",brush = brushOpts(id = "brush",resetOnNew = TRUE))),
            box(title="Scatterplot", div(class="info-icon", id="scatterplot-info", icon("info-circle")), width=12,solidHeader=TRUE,plotOutput("scatterplot")),
            auto.advance=FALSE 
          )
        ),
        tabPanel("Model",
          carouselPanel(
            box(id="model1_box",title="K Means Clustering", div(class="info-icon", id="kmeans-info", icon("info-circle")), width=12,solidHeader = TRUE,
              div(class="control-row", style="width: 100%; clear: both;", 
                div(style="float: left; padding-right:8px; width: 25%;",uiOutput("clust_dep")),
                div(style="float: left; padding-right:8px; width: 25%;",uiOutput("clust_indep")),
                div(style="float: left; padding-right:8px; width: 25%;",numericInput('clusters', 'Cluster count', 3,min = 1, max = 9))
              ), 
              plotOutput('model1'),
              hidden(
                div(id="model1_i",
                    box( title="More Information", width=12,
                         verbatimTextOutput("model1_info")   
                    )
                )
              )
            ),
            box(id="model2_box",title="Bivariate Linear Regression", div(class="info-icon", id="bivariate-info", icon("info-circle")), width=12,solidHeader = TRUE,
              div(class="control-row", style="width: 100%; clear: both;",
                div(style="display:inline-block; padding-right:8px;",uiOutput("lm_dep")),
                div(style="display:inline-block; padding-right:8px;",uiOutput("lm_indep"))
              ), 
              plotOutput('model2'),
              hidden(
                div(id="model2_i",
                  div(id="model2_i1",
                        box( title="More Information",width=7,
                             verbatimTextOutput("model2_info")   
                        )
                  ),
                  div(id="model2_i2",
                      box( title="Residuals",width=5,
                           plotOutput("model2_resid") 
                      )   
                  )
                )
              )
            ),   

            box(id="model3_box",title="Decision Tree", div(class="info-icon", id="decisiontree-info", icon("info-circle")), width=12,solidHeader = TRUE,
                div(class="control-row", style="width: 100%; clear: both;",
                  div(style="display:inline-block; padding-right:8px;",uiOutput("tree_dep")),
                  div(style="display:inline-block; padding-right:8px;",uiOutput("tree_indep"))
                ), 
                plotOutput('model3'),
                hidden(
                  div(id="model3_i",
                      box( title="More Information",width=12,
                           verbatimTextOutput("model3_info")   
                      )
                  )
                )
                
            ),
            auto.advance=FALSE
          )
        )
      ),
      HTML("</div>")
    ),
    div(style="display:inline-block",selectInput("plots", "Choose Plot(s)", choices=list("Boxplot","Density Plot","Scatter Plot","K-Means","Linear Model","Decision Tree"), multiple=TRUE)),
    div(style="display:inline-block;",downloadButton('downloadPlots')),
    bsCollapsePanel("View Data",bsCollapsePanel("Summary",verbatimTextOutput("summary")), DT::dataTableOutput('mydata'))
  )
)