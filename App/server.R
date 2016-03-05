palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

shinyServer(function(input, output, session) {
  
  #############################
  # DATA INPUT + MANIPULATION #
  #############################
  
  # EXPORT DATA TO TABLE
  output$mydata <- renderDataTable({
    inFile <- input$file1
    if (is.null(inFile)) {
      tbl<-(iris)
    }
    else{
      tbl <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)  
    }
    return (tbl)
  },options=list(lengthMenu=c(10,20,30,40,50),pageLength=10))
  
  output$choose_columns <- renderUI({
    if(is.null(input$dataset)){
      colnames<-names(iris)
    }
    else{
      colnames <- names(tbl) 
    }
    checkboxGroupInput("columns", "Choose columns", choices  = colnames)
  })
  
  #################
  # VISUALIZATION #
  #################
  
  # BOXPLOTS
  output$boxplot<-renderPlot({
    boxplot(iris$Sepal.Length)
  })
  
  #################
  #    MODELING   #
  #################
  
  # K-MEANS CLUSTERING
  selectedData <- reactive({iris[, c(input$xcol, input$ycol)]})
  clusters <- reactive({kmeans(selectedData(), input$clusters)})
  output$kmeans <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(), col = clusters()$cluster, pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  
})