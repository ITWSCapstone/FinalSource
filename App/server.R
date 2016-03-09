palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

shinyServer(function(input, output, session) {
  
  #############################
  # DATA INPUT + MANIPULATION #
  #############################
  tbl<-reactive({ #tbl()=user input or sample iris data
    inFile <- input$file1
    if (is.null(inFile)) {
      return (iris)
    }
    else{
      read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)  
    }
  })
  

  # EXPORT DATA TO TABLE
  output$mydata <- DT::renderDataTable({
    return (tbl())
  },extensions = 'TableTools',options = list(searchHighlight=TRUE,rownames=FALSE,
      "sDom" = 'T<"clear">lfrtip',"oTableTools" = list("sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
      "aButtons" = list("copy","print",list("sExtends" = "collection","sButtonText" = "Save","aButtons" = c("csv","xls")))
    )
  ))
  

  #################
  # VISUALIZATION #
  #################
  
  # BOXPLOTS
  output$choose_column1 <- renderUI({
    selectInput("column1", "Choose Column(s)", names(tbl()), multiple=TRUE, selected=list(names(tbl())[[1]],names(tbl())[[2]]))
  })
  output$choose_column2 <- renderUI({
    selectInput("column2", "Choose Grouping", names(tbl()))
  })
  output$boxplot<-renderPlot({
    b<-tbl()[input$column1]
    boxplot(b)
  })
  
  output$heatmap<-renderPlot({
    validate( #error handling
      need(length(input$column1) == 2, " * Please select 2 columns")
    )
    mat<-data.matrix(tbl()[input$column1])
    print (mat)
    heatmap(mat)
  })
  
  output$scatterplot<-renderPlot({
    plot(tbl()[input$column1])
  })
  
  #################
  #    MODELING   #
  #################
  
  # K-MEANS CLUSTERING
  output$choose_kmeansX <- renderUI({
    selectInput('xcol', 'X Variable', names(tbl()))
  })
  output$choose_kmeansY <- renderUI({
    selectInput('ycol', 'Y Variable', names(tbl()),selected=names(tbl())[[2]])
  })
  selectedData <- reactive({tbl()[, c(input$xcol, input$ycol)]})
  clusters <- reactive({kmeans(selectedData(), input$clusters)})
  output$kmeans <- renderPlot({
    validate( #error handling
      need(!is.na(tbl()[input$xcol]) && !is.na(tbl()[input$ycol]), " * Non-Numeric Column-please reformat data or select a different column")
    )
  
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(), col = clusters()$cluster, pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  
})