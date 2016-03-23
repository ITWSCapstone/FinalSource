palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

options(shiny.maxRequestSize = 15*1024^2) #server file input limit (15MB)
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
    }
  })
  
  TheTS<-reactive({
    # RETURNS SELECTED (ACTUALS) AS TIME SERIES
    if (is.null(input$timecol))
      return (NULL)
    data=tbl()
    colindex=grep(input$timecol, colnames(data)) #returns column index of column user selects
    a1=as.numeric(as.character(data[,colindex]))
    curr=data.frame(a1)
    curr=curr[rowSums(is.na(curr)) != ncol(curr),]
    thets=ts(curr, start=c(2011,50) , frequency=52) #<<<<<<<<<NEED TO DYNAMICALLY SET DATE RANGE SOMEHOW.....
    thets
    
  })
  TheETS<-reactive({
    #returns selcted series as ETS for dygraphs
    if (is.null(input$timecol))
      return (NULL)
    data=tbl()
    colindex=grep(input$timecol, colnames(data))
    
    #ACTUALS
    act=as.numeric((data[,colindex]))
    act=data.frame(act)
    act=act[rowSums(is.na(act)) != ncol(act),]
    
    #FITTED
    fit=as.numeric((data[,colindex+1]))
    fit=data.frame(fit)
    fit=fit[rowSums(is.na(fit)) != ncol(fit),] #remove NAs
    
    dates=as.character(data[,2]) #HARD CODED UGH
    dates=as.Date((dates),"%B/%d/%Y") #Will need all dates to be formatted as such
    dates=head(dates, length(act))
    ret=xts(cbind(act,fit),order.by = dates)
    ret
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
  rplot <- reactiveValues(x = NULL, y = NULL)
  
  # BOXPLOTS
  output$cols1 <- renderUI({
    selectInput("cols1", "Choose Column(s)", names(tbl()), multiple=TRUE, selected=list(names(tbl())[[1]],names(tbl())[[2]]))
  })
  output$groupcol <- renderUI({
    selectInput("groupcol", "Group By", names(tbl()),selected=list(names(tbl())[[5]]))
  })
  output$timecol <- renderUI({
    selectizeInput(
      'timecol', 'Choose Time Series', choices = names(tbl()),
      multiple = TRUE, options = list(maxItems = 1)
    )
  })
  output$boxplot<-renderPlot({
    b<-tbl()[input$cols1]
    boxplot(b)
  })
  
  # DENSITY GRAPH
  output$densityplot<-renderPlot({
    mdat<-melt(tbl()[input$cols1])
    ggplot(mdat, aes(value)) +
      geom_histogram(aes(y=..density..), binwidth=5, colour='black', fill='skyblue') + 
      geom_density() + 
      facet_wrap(~variable, scales="free")+
      coord_cartesian(xlim = rplot$x, ylim = rplot$y)
  })
  observeEvent(input$plotdblclick, { #Dynamic range (drag and double click to resize graph)
    brush <- input$brush
    if (!is.null(brush)) {
      rplot$x <- c(brush$xmin, brush$xmax)
      rplot$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rplot$x <- NULL
      rplot$y <- NULL
    }
  })
  
  # SCATTERPLOT
  output$scatterplot<-renderPlot({
    if(is.null(TheTS())) { plot(tbl()[input$cols1])}
    else{
      TS=TheTS()
      plot(TS, main=paste(input$vars, sep= " "),xlab="Time", ylab=input$timecol,col="purple", type =input$dataplotType)
    }
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
    #validate(
      #need(length(input$cols2)==2, "Please select two columns")
    #)
    ggplot(tbl(), aes(x=tbl()[input$cols2[1]], y=tbl()[input$cols2[2]])) + 
      geom_point()+
      geom_smooth(method=lm)+
      labs(x = input$cols2[1],y = input$cols2[2])
  })
  observeEvent(input$model2click, { #Display test results on click
    output$model2_info<-renderPrint({
      mod<-lm(as.formula(paste(input$cols2[1]," ~ ",paste(input$cols2[2],collapse="+"))),data=tbl())
      print(summary(mod))
    })
    output$model2_resid<-renderPlot({
      mod<-lm(as.formula(paste(input$cols2[1]," ~ ",paste(input$cols2[2],collapse="+"))),data=tbl())
      plot(resid(mod))
      abline(0,0, col="red")
    })
    show("model2_i", anim=TRUE)
    hide("model2", anim=TRUE)
  })
  onclick("model2_i", show("model2"))
  
  output$model3<-renderPlot({
    if(is.null(TheETS())) { return(NULL)}
    TS=TheETS()
    names(TS)=c("Actuals", "Fitted")
    dygraph(TS, main=paste(input$timecol, "Forecast", sep= " ")) %>% dyRangeSelector()
  })
  
  
})