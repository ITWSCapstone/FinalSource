palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

shinyServer(function(input, output, session) {
  source("functions.R")
  #############################
  # DATA INPUT + MANIPULATION #
  #############################
  tbl<-reactive({ #tbl()=user input or sample iris data
    inFile <- input$file1
    if (is.null(inFile)) {
      return(iris)
    }
    else{
      csv<-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)  
      sapply(csv, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))
      
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
  
  output$summary<-renderPrint({
    summary(tbl())
  })
  

  #################
  # VISUALIZATION #
  #################
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # BOXPLOTS
  output$cols1 <- renderUI({
    selectInput("cols1", "Choose Column(s)", names(tbl()), multiple=TRUE, selected=list(names(tbl())[[1]],names(tbl())[[2]]))
  })
  output$groupcol <- renderUI({
    selectInput("groupcol", "Group By", names(tbl()),selected=list(names(tbl())[[5]]))
  })
  output$timecol <- renderUI({
    selectInput("timecol", "Choose Time Series", names(tbl()))
  })
  output$boxplot<-renderPlot({
    b<-tbl()[input$cols1]
    boxplot(b)
  })
  
  
  # DENSITY GRAPH
  output$densityplot<-renderPlot({
    library(reshape2)
    mdat<-melt(tbl()[input$cols1])
    ggplot(mdat, aes(value)) +
      geom_histogram(aes(y=..density..), binwidth=5, colour='black', fill='skyblue') + 
      geom_density() + 
      facet_wrap(~variable, scales="free")
  })
  
  
  # SCATTERPLOT
  output$scatterplot<-renderPlot({
    plot(tbl()[input$cols1])
  })

  
  #################
  #    MODELING   #
  #################
  
  # K-MEANS CLUSTERING
  output$cols2 <- renderUI({
    selectInput("cols2", "Choose Column(s)", names(tbl()), multiple=TRUE, selected=list(names(tbl())[[1]],names(tbl())[[2]]))
  })
  selectedData <- reactive({tbl()[, c(input$cols2[1], input$cols2[2])]})
  clusters <- reactive({kmeans(selectedData(), input$clusters)})
  output$model1 <- renderPlot({
    validate( #error handling
      need(!is.na(tbl()[input$cols2[1]]) && !is.na(tbl()[input$cols2[2]]), " * Non-Numeric Column-please reformat data or select a different column")
    )
  
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(), col = clusters()$cluster, pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  # LINEAR REGRESSION
  output$model2<-renderPlot({
    validate(
      need(length(input$cols2)==2, "Please select two columns")
    )
    ggplot(tbl(), aes(x=tbl()[input$cols2[1]], y=tbl()[input$cols2[2]])) + 
      geom_point()+
      geom_smooth(method=lm)+
      labs(x = input$cols2[1],y = input$cols2[2])+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  })
  observeEvent(input$plotdblclick, { #Dynamic range (drag and double click to resize graph)
    brush <- input$brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
})