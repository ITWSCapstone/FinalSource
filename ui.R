library("shiny")
library("forecast")
library("dygraphs")
library("xts")
library("wordcloud")
library("dynlm")
library("ggplot2")
library("devtools")
#devtools::install_github("dvanclev/GTrendsR")
library("gtrendsR")


file1=NULL

names=c("Data Input",
        "Charting & Visualization",
        "Baseline Forecast",    #arima
        "Diagnostics",  # MLR
          "Decomposition",          
          "Residual Analysis",      
        "Key Driver Analysis",
          "Category Mouthwash Simulation",
          "Listerine Simulation",
          "Category Mouthwash Waterfalls",
          "Listerine Waterfalls",
        "Google Trends")


shinyUI(navbarPage("Consumer Forecasting",
                   tabPanel(names[1],
                            sidebarLayout(
                              sidebarPanel(
                                fileInput('file1', 'Choose file to upload',
                                          accept = c(
                                            'text/csv',
                                            'text/comma-separated-values',
                                            'text/tab-separated-values',
                                            'text/plain',
                                            '.csv',
                                            '.tsv'
                                          )
                                ),
                                tags$hr(),
                                
                                
                                selectInput("vars", "Select Time Series to Analyze",
                                                   choices="Pending Upload")   
                                
                                
                                
                              ), #end sidebar panel
                              
                              mainPanel(
                                textOutput('userpicks'),
                                tableOutput('contents')
                                
                              )
                            )
                            
                            
                            
                            
                            
                   ),#end data input tabpanel
                   
                   
                   
                   tabPanel(names[2],
                            sidebarLayout(
                              sidebarPanel(
                                strong("Your Data"),
                                radioButtons("dataplotType", "Plot type",
                                             c("Scatter"="p", "Line"="l")
                                )
                                
                              ),
                              mainPanel(
                                dygraphOutput("tsPlot"),
                                plotOutput("tsCyclePlot"),
                                plotOutput("tsPlot1")
                                
                              )
                            )
                   ),#end analyzed data tabpanel
                   
                   
                   
                   tabPanel(names[3],
                            sidebarLayout(
                              sidebarPanel(
                                strong("Baseline forcast"),
                                p("ARIMA is a forecasting technique that projects the future values of a series based entirely on its own patterns."),
                                p("It works best when your data exhibits a stable or consistent pattern over time with a minimum amount of agressive changes")
                                #downloadLink('downloadData', 'Download Arima Forecast Data')
                              ),
                              mainPanel(
                                dygraphOutput("forecastPlot")
                              )
                            )                 
                   ),#end arima tabpanel
                   
                   
                   
                   navbarMenu(names[4],
                              
                              
                              tabPanel(names[5],
                                       sidebarLayout(
                                         sidebarPanel(
                                           checkboxInput("robust", "Robust to outliers", FALSE),
                                           
                                           strong("Individual Components"),
                                           p("This panel looks to examine the separate components of a dataset"),
                                           p("The first panel looks at the plot of the data itself"),
                                           p("The second looks for a long term trend: upwards, downwards, or combiniations of both."),
                                           p("The third looks for any seasonality that may be present in the data."),
                                           p("The fourth looks how much of the data is random, or can't be explained by those two factors. A sharp spike may indicate an outlier in the dataset.")
                                         ),
                                         mainPanel(
                                           plotOutput("decompPlot")
                                         )
                                       )                   
                              ),#end decomp tabpanel
                              
                              
                              tabPanel(names[6],
                                       sidebarLayout(
                                         sidebarPanel(
                                           h3("Residuals"),
                                           radioButtons("resplotType", "Plot type",
                                                        c("Scatter"="p", "Line"="l")
                                           )
                                         ),
                                         mainPanel(
                                           plotOutput("residualPlot"),
                                           br(),
                                           plotOutput("acfPlot")
                                         )
                                       )                   
                                       
                              )#end residuals tabpanel
                              
                     ), #end diagnostics menu         
                   
                   
                   
                    navbarMenu(names[7],                 
                                     
                                     tabPanel(names[8],
                                              sidebarLayout(
                                                sidebarPanel(
                                                  strong("Modeled Forecast"),
                                                  p("This forcasting technique attempts to model the effects of two or more explanatory variables on some response variable"),
                                                  p("In particular, we seek to model the effects of GDP and Price on the Sales data"),
                                                  
                                                  selectInput("num029observ", "Model includes data up until:",
                                                              choices="Pending Upload"),   
                                                  
                                                  
                                                  selectInput("dlmindex", "Run dynamic regression on:", c("National"="A_National",
                                                                                                          "Drug"="A_Drug",
                                                                                                          "Channel"="A_Channel",
                                                                                                          "Conventional"="A_Conventional",
                                                                                                          "Discount"="A_Discount")),
                  
                  
                                                  sliderInput("priceslider", label = "Price Multiplier", min = 0, 
                                                              max = 2, value = 1, step=0.005)
                                                  
                                                  
                                                  
                                                  
                                                  #downloadLink('downloadmlrData', 'Download MLR Forecast Data')
                                                  
                                                ),
                                                mainPanel(
                  
                                                  plotOutput("dlmPlot"),#actuals vs fitted
                                                  plotOutput("dlmforecastplot"),#price mult =1
                                                  plotOutput("dlmsimulatedplot"),# simulation
                                                  plotOutput("diff029plot"),#difference
                                                  tableOutput("dlmTable")  #values
                                                  
                                                )
                                              )                   
                                     ),#end category mouthwash simulation panel

                              tabPanel(names[9],
                                       sidebarLayout(
                                         sidebarPanel(
                                           strong("Modeled Forecast"),
                                           p("This forcasting technique attempts to model the effects of two or more explanatory variables on some response variable"),
                                           p("In particular, we seek to model the effects of GDP and Price on the Sales data"),
                                           
                                           selectInput("num031observ", "Model includes data up until:",
                                                       choices="Pending Upload"),   
                                           
                                           
                                           
                                           selectInput("unitselected", "Select units", c("Unit"=1, "Tonnage"=2 )),
                                           
                                           
                                           sliderInput("priceslider29", label = "Price Multiplier on 029", min = 0, 
                                                       max = 2, value = 1, step=0.005),
                                           
                                           sliderInput("priceslider31", label = "Price Multiplier on 031", min = 0, 
                                                       max = 2, value = 1, step=0.005)
                                           #downloadLink('downloadmlrData', 'Download MLR Forecast Data')
                                           
                                         ),
                                         mainPanel(
                              
                                           plotOutput("dlm2Plot"), #actuals vs fitted
                                           plotOutput("dlm2FixedFCPlot"), #price mult =1
                                           plotOutput("dlm2FCPlot"),  # simulation
                                           plotOutput("dlm2FCdiffPlot"), #difference
                                           tableOutput("dlm2Table") #values
                                         )
                                       )                   
                              ), #end listerine simulation panel
                            
                              
                            tabPanel(names[10],
                                     sidebarLayout(
                                       sidebarPanel(
                                         strong("Category: National Mouthwash")
                                       ),
                                       mainPanel(
                                         plotOutput("wf029"),
                                         plotOutput("bwwf029")
                                         
                                       )
                                     )
                                     
                                     
                                     
                                     ),#end category mouthwash waterfalls panel
                  
                  
                            
                            tabPanel(names[11],
                                     sidebarLayout(
                                       sidebarPanel(
                                         strong("Listerine")
                                         ),
                                       mainPanel(
                                          
                                          plotOutput("wf031"),
                                          plotOutput("bwwf031")
                                       )
                                     )
                            )#end listerine waterfalls panel
                  
                  ),#end driver menu

                   tabPanel(names[12],
                            sidebarPanel(
                              
                              textInput('query', "Enter search term"),
                              
                              dateInput('date1',
                                        label = 'Enter Start Date: (Sunday)',
                                        format = "mm/dd/yy",
                                        startview = 'year',
                                        value = Sys.Date()
                              ),
                              dateInput('date2',
                                        label = 'Enter End Date: (Sunday)',
                                        format = "mm/dd/yy",
                                        startview = 'year',
                                        value = Sys.Date()
                              ),
                              radioButtons("googleplotType", "Plot type",
                                           c("Scatter"="p", "Line"="l")
                              )
                            ),
                            
                            mainPanel(
#                               textOutput('testquery'),
#                               textOutput('testdate1'),
#                               textOutput('testdate2'),
                              plotOutput('trendplot'),
                              plotOutput('searchWordCloud')
                              
                              
                            )     
                   )#end google trends tab panel
                   
)
)