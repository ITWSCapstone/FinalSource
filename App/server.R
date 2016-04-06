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
    colindex=grep(input$timecol, names(data)) #returns column index of column user selects
    a1=as.numeric(as.character(data[,colindex]))
    curr=data.frame(a1)
    curr=curr[rowSums(is.na(curr)) != ncol(curr),]
    thets=ts(curr, start=c(2011,50) , frequency=52) #<<<<<<<<<NEED TO DYNAMICALLY SET DATE RANGE SOMEHOW.....
    thets
    
  })
  TheETS<-reactive({
    #returns selcted series as ETS for dygraphs
    if (is.null(input$timecol)){
      return (NULL)
    }
    data=tbl()
    colindex=grep(input$timecol, names(data))
    
    #ACTUALS
    act=as.numeric((data[,colindex]))
    act=data.frame(act)
    act=act[rowSums(is.na(act)) != ncol(act),]
    
    #FITTED
    fit=as.numeric((data[,colindex+1]))
    fit=data.frame(fit)
    fit=fit[rowSums(is.na(fit)) != ncol(fit),] #remove NAs
    
    dateindex=grep("Period",names(data))
    dates=as.character(data[,dateindex]) #HARD CODED UGH
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
  
  output$summary<-renderPrint({summary(tbl())})
  

  #################
  # VISUALIZATION #
  #################
  rplot <- reactiveValues(x = NULL, y = NULL)
  # BOXPLOTS
  output$vis <- renderUI({
    numeric <- sapply(tbl(), is.numeric)
    selectInput("vis", "Choose Column(s)", names(tbl()[numeric]), multiple=TRUE, selected=list(names(tbl()[numeric])[[1]],names(tbl()[numeric])[[2]]))
  })
  output$groupcol <- renderUI({
    categorical <- !sapply(tbl(), is.numeric) #only group by categorical fields
    selectizeInput(
      'groupcol', 'Group By', choices = names(tbl()[categorical]),
      multiple = TRUE, options = list(maxItems = 1)
    )
  })
  
  bplot = function() {
    b<-tbl()[input$vis]
    boxplot(b)
  }
  output$boxplot<-renderPlot({print(bplot())})
  
  # DENSITY GRAPH
  dplot = function() {
    mdat<-melt(tbl()[input$vis])
    ggplot(mdat, aes(value)) +
      geom_histogram(aes(y=..density..), binwidth=5, colour='black', fill='skyblue') + 
      geom_density() + 
      facet_wrap(~variable, scales="free")+
      coord_cartesian(xlim = rplot$x, ylim = rplot$y)
  }
  output$densityplot<-renderPlot({print(dplot())})
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
  splot = function() {
    if(is.null(TheTS())) { 
      if(is.null(input$groupcol)){
        plot(tbl()[input$vis])  
      }
      else{
        plot(tbl()[input$vis],col=tbl()[[input$groupcol]]) 
        legend ("topleft", legend = levels(tbl()[[input$groupcol]]), col = c(1:3), pch = 16)
      }
    }
    else{
      TS=TheTS()
      plot(TS, main=paste(input$timecol, sep= " "),xlab="Time", ylab=input$timecol,col="purple")
    }
  }
  output$scatterplot<-renderPlot({print(splot())})
  
  #################
  #    MODELING   #
  #################
  
  # K-MEANS CLUSTERING
  output$clust_indep <- renderUI({
    numeric <- sapply(tbl(), is.numeric)
    selectInput("clust_indep", "Independent Variable", names(tbl()[numeric]), multiple=FALSE, selected=list(names(tbl()[numeric])[[2]]))
  })
  output$clust_dep <- renderUI({
    numeric <- sapply(tbl(), is.numeric)
    selectInput("clust_dep", "Dependent Variable", names(tbl()[numeric]), multiple=FALSE, selected=list(names(tbl()[numeric])[[1]]))
  })
  selectedData <- reactive({tbl()[, c(input$clust_dep, input$clust_indep)]})
  clusters <- reactive({kmeans(selectedData(), input$clusters)})
  cplot=function(){
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(), col = clusters()$cluster, pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  }
  output$model1<-renderPlot({print(cplot())})
  output$model1_info<-renderTable({
    table(clusters()$cluster, input$clust_dep)
  })
  shinyjs::onclick("model1",toggle("model1_i", anim=TRUE))
  shinyjs::onclick("model1_i",toggle("model1", anim=TRUE))
  
  # LINEAR REGRESSION
  output$lm_indep <- renderUI({
    numeric <- sapply(tbl(), is.numeric)
    selectInput("lm_indep", "Independent Variable", names(tbl()[numeric]), multiple=FALSE, selected=list(names(tbl()[numeric])[[2]]))
  })
  output$lm_dep <- renderUI({
    numeric <- sapply(tbl(), is.numeric)
    selectInput("lm_dep", "Dependent Variable", names(tbl()[numeric]), multiple=FALSE, selected=list(names(tbl()[numeric])[[1]]))
  })
  output$model2<-renderPlot({
    ggplot(tbl(), aes(x=tbl()[input$lm_indep], y=tbl()[input$lm_dep])) + 
      geom_point()+
      geom_smooth(method=lm)+
      labs(x = input$lm_indep,y = input$lm_dep)
  })
  output$model2_info<-renderPrint({
    mod<-lm(as.formula(paste(input$lm_dep," ~ ",paste(input$lm_indep,collapse="+"))),data=tbl())
    print(summary(mod))
  })
  output$model2_resid<-renderPlot({
    mod<-lm(as.formula(paste(input$lm_dep," ~ ",paste(input$lm_indep,collapse="+"))),data=tbl())
    plot(resid(mod))
    abline(0,0, col="red")
  })
  shinyjs::onclick("model2",toggle("model2_i", anim=TRUE))
  shinyjs::onclick("model2_i",toggle("model2", anim=TRUE))
  
  # Decision Tree
  output$tree_indep <- renderUI({
    selectInput("tree_indep", "Independent Variable(s)", names(tbl()[ , names(tbl()) != input$tree_dep]), multiple=TRUE, selected=list(names(tbl()[ , names(tbl()) != input$tree_dep])[[2]]))
  })
  output$tree_dep <- renderUI({
    selectInput("tree_dep", "Dependent Variable", names(tbl()), multiple=FALSE, selected=list(names(tbl())[[1]]))
  })
  output$model3<-renderPlot({
    stree = ctree(as.formula(paste(input$tree_dep," ~ ",paste(input$tree_indep,collapse="+"))), data = tbl())
    plot(stree)
  })
  
  # ARIMA
  output$arima_indep <- renderUI({
    numeric <- sapply(tbl(), is.numeric)
    selectInput("arima_indep", "Independent Variable(s)", names(tbl()[numeric]), multiple=TRUE, selected=list(names(tbl()[numeric])[[2]]))
  })
  output$arima_dep <- renderUI({
    numeric <- !sapply(tbl(), is.numeric)
    selectInput("arima_dep", "Dependent Variable", names(tbl()[numeric]), multiple=FALSE, selected=list(names(tbl()[numeric])[[1]]))
  })
  output$timecol <- renderUI({
    selectizeInput(
      'timecol', 'Choose Time Series', choices = names(tbl()),
      multiple = TRUE, options = list(maxItems = 1)
    )
  })
  output$model4<-renderPlot({
    if(is.null(TheETS())) { return(NULL)}
    TS=TheETS()
    names(TS)=c("Actuals", "Fitted")
    dygraph(TS, main=paste(input$timecol, "Forecast", sep= " ")) %>% dyRangeSelector()
  })
  
  
  #########################################
  #            SAVE PLOT(S)               #
  #########################################
  output$downloadPlots <- downloadHandler( filename = function() {paste(format(Sys.Date, "%d/%b/%Y"),"-Report.pdf")},content = function(file) {
    pdf(file) 
    for (i in 1:length(input$plots)){
      if(input$plots[i]=="Boxplot")
        print (bplot())
      if(input$plots[i]=="Density Plot")
        print (dplot())
      if(input$plots[i]=="Scatter Plot")
        print(splot())
    }
    dev.off()})
  
}) 