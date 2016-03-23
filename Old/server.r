#how to input file
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 15MB.

options(shiny.maxRequestSize = 15*1024^2)

shinyServer(function(session, input, output) {
  
  
  #input data
  Dataset <- reactive({
    infile=input$file1
    if (is.null(input$file1)) {
      return(NULL)
    }
    upload=read.csv(infile$datapath, header = TRUE)
    #     MMD[MMD==""]=NA
    #     MMD=MMD[rowSums(is.na(MMD)) != ncol(MMD),]
    #     MMD=MMD[,colSums(is.na(MMD)) != nrow(MMD)]
    return (data.frame(upload) )
  })
  
  invs <- reactive ({
    if (is.null(input$file1)) {
      return(NULL)
    }
    alldata=Dataset()
    data=alldata[,c(37,43,44,51,62:68)]
    return(data)
    
  })
  
  invs2 <- reactive ({
    if (is.null(input$file1)) {
      return(NULL)
    }
    alldata=Dataset()
    data=alldata[,c(37,43,66,49)]
    return(data)
    
  })
  

  
  TheTS<-reactive({
    # RETURNS SELECTED (ACTUALS) AS TIME SERIES
    if (is.null(Dataset()))
      return (NULL)
    
    data=Dataset();
    colindex=grep(input$vars, colnames(data)) #returns column index of column user selects
    
    a1=as.numeric(as.character(data[,colindex]))
    curr=data.frame(a1)
    curr=curr[rowSums(is.na(curr)) != ncol(curr),]
    
    thets=ts(curr, start=c(2011,50) , frequency=52)
    thets
    
  })
  
  
  TheTS1<-reactive({
    # RETURNS (SELECTED + 1) (FITTED) AS TIME SERIES
    if (is.null(Dataset()))
      return (NULL)
    
    data=Dataset();
    colindex=grep(input$vars, colnames(data)) #returns column index of column user selects
    
    a1=as.numeric(as.character(data[,colindex+1]))
    curr=data.frame(a1)
    curr=curr[rowSums(is.na(curr)) != ncol(curr),]
    
    thets=ts(curr, start=c(2011,50) , frequency=52)
    thets
    
  })
  
  
  TheETS<-reactive({
    #returns selcted series as ETS (needed for dygraphs)
    #ret[,1] for actuals, ret[,2] for fitted
    if (is.null(Dataset()))
      return (NULL)
    
    data=Dataset();
    colindex=grep(input$vars, colnames(data)) #returns column index of column user selects
    
    #ACTUALS
    #get column data
    act=as.numeric((data[,colindex]))
    #now a data frame so can remove NAs
    act=data.frame(act)
    #becomes a vector again
    act=act[rowSums(is.na(act)) != ncol(act),]
    
    #FITTED
    #get column data
    fit=as.numeric((data[,colindex+1]))
    #now a data frame so can remove NAs
    fit=data.frame(fit)
    #becomes a vector again
    fit=fit[rowSums(is.na(fit)) != ncol(fit),]
    
    
    
    #    currts=ts(curr, start=c(2011,50), frequency=52)      
    #   as.Date(as.character(data[2,2]),"%B/%d/%Y")
    
    dates=as.character(data[,2])
    dates=as.Date((dates),"%B/%d/%Y")
    dates=head(dates, length(act))
    
    ret=xts(cbind(act,fit),order.by = dates)
    ret
    
  })
  
  
  
  output$varselect <- renderUI({ })
  
  
  observe({
    if (is.null(input$file1)) {
      return(NULL)
    }
    
    updateSelectInput(session, inputId="vars", label="Select Time Series to Analyze",
                      choices=names(Dataset()), selected =NA )
    data=Dataset()
    dateindex=as.numeric(grep("Period",colnames(data)))
    
    
    updateSelectInput(session, inputId="num029observ", label="Model includes data up until:",
                      choices=as.character((data[,as.numeric(dateindex)])), selected ="Dec/13/2014" )
    
    updateSelectInput(session, inputId="num031observ", label="Model includes data up until:",
                      choices=as.character((data[,as.numeric(dateindex)])), selected ="Dec/13/2014" )
    
  })
  
  
  output$userpicks<- renderText({ input$vars })
  

  
  #my data
  output$contents <- renderTable({
    #if selection isnt made yet, display entire data set
    #when selection is made display actuals and forecast for selected
    if (input$vars=="") { return (Dataset()) } else 
      
      if (is.null(input$file1)) {
        return(NULL)
      } 
    
    data=Dataset();
    colindex=grep(input$vars, colnames(data)) #returns column index of column user selects
    
    actuals=as.numeric(as.character(data[,colindex]))
    forecasts=as.numeric(as.character(data[,colindex+1]))
    curr=data.frame(actuals,forecasts)
    #remove NA's/trailing blanks
    curr[curr==""]=NA
    curr=curr[rowSums(is.na(curr)) != ncol(curr),]
    curr=curr[,colSums(is.na(curr)) != nrow(curr)]
    
    currdf=data.frame(data[1:nrow(curr),2], curr)
    names(currdf)=c("Period", input$vars , "Fitted")
    return (currdf)
  })
  
  output$currentcontents <- renderTable({
    MMD=Dataset()
    MMD
    
  })

  output$tsPlot <- renderDygraph({
    
    if(is.null(TheETS())) { return(NULL)}
    TS=TheETS()
    
    #plot actuals
    dygraph(TS[,1], main=paste(input$vars, "data", sep= " ")) %>% dyRangeSelector()
    
  })
  
  
  output$tsPlot1 <- renderPlot({
    
    if(is.null(TheTS())) { return(NULL)}
    TS=TheTS()
    plot(TS, main=paste(input$vars, "data", sep= " "),xlab="Time", ylab="Volume",col="purple", type =input$dataplotType)
    
    #     THEXTS=as.xts(TS)
    #     plot(THEXTS, type=input$dataplotType, main=paste(input$vars, "data", sep= " "))
    #     
  })
  
  output$tsCyclePlot <- renderPlot({
    
    if(is.null(TheTS())) { return(NULL)}
    TS=TheTS()
    
    
    seasonplot(TS, year.labels=TRUE, year.labels.left=TRUE, col=2:20,
               main=paste(input$vars, "Cycle Charting", sep= " "), xlab="Week" )
    
  })
  
  
  output$forecastPlot=renderDygraph({
    
    #use forecast(For_List_ar)$method   for name
    
    if(is.null(TheTS())) { return(NULL)}
    TS=TheETS()
    names(TS)=c("Actuals", "Fitted")
    
    dygraph(TS, main=paste(input$vars, "Forecast", sep= " ")) %>% dyRangeSelector()
    
    
  })
  
  
  
  
  
  output$dlmPlot=renderPlot({
    #actuals vs fitted
    #DYNAMIC MODELS      
    if(is.null(TheTS())) { return(NULL)}
    
    data=Dataset()
    salesindex=as.numeric(grep(as.character(input$dlmindex), colnames(data))) #index for sales/actuals
    
    
    selecteddate=as.character(input$num029observ)
    daterowindex=grep(selecteddate,data[,grep("Period",colnames(data))])

    
    sales=data[,salesindex]
    sales=head(sales,as.numeric(daterowindex))
    salests=ts(sales, start=c(2011,50), frequency=52)      
    
    price=data[,(salesindex+2)]
    price=head(price,as.numeric(daterowindex))
    pricets=ts(price, start=c(2011,50), frequency=52)      
    
    gdpindex=as.numeric(grep("GDP",colnames(data)))
    gdp=data[,gdpindex]
    gdp=head(gdp,as.numeric(daterowindex))
    gdpts=ts(gdp, start=c(2011,50), frequency=52)      
    
    m1=dynlm(salests~pricets+gdpts+L(salests, 1) + L(salests, 2)) #model
    fit=predict(m1)
    fit=ts(fit,start=c(2011,52), frequency = 52)
    
    plot(fit, type="l", col="red", ylab="Sales", main="Actuals and Model Fitted Values") #fitted
    lines(salests, col="blue") #actuals
    legend("top", c("Actuals","Fitted"), lty=c(1,1),  lwd=c(2.5,2.5) ,col=c("blue","red"))
    
  })
  

  output$dlmTable=renderTable({
    if(is.null(TheTS())) { return(NULL)}
    
    data=Dataset()
    salesindex=as.numeric(grep(as.character(input$dlmindex), colnames(data))) #index for sales/actuals
    selecteddate=as.character(input$num029observ)
    daterowindex=grep(selecteddate,data[,grep("Period",colnames(data))])
    
    
    
    sales=data[,salesindex]
    sales=head(sales,as.numeric(daterowindex))
    salests=ts(sales, start=c(2011,50), frequency=52) 
    
    price=data[,(salesindex+2)]
    price=head(price,as.numeric(daterowindex))
    pricets=ts(price, start=c(2011,50), frequency=52)
    
    gdpindex=as.numeric(grep("GDP",colnames(data)))
    gdp=data[,gdpindex]
    gdp=head(gdp,as.numeric(daterowindex))
    gdpts=ts(gdp, start=c(2011,50), frequency=52)      
    
    m1=dynlm(salests~pricets+gdpts+L(salests, 1) + L(salests, 2)) #model
    fit=predict(m1)
    fit=ts(fit,start=c(2011,52), frequency = 52)
  

    fcasts=c()
    for(iter in (as.numeric(daterowindex)+1):nrow(data)){ 
      #intercept, price, gdp, lag1, lag 2
      value=coef(m1) %*% c(1, ((input$priceslider)*(data[iter,(salesindex+2)])), data[iter,gdpindex],data[(iter-1),salesindex],data[(iter-2),salesindex])
      fcasts=c(fcasts,value)
    }
    
    #figure out where to start forcasts as time series
    if(end(gdpts)[2]==52){
      obsr=1
      year=end(gdpts)[1]+1
      
    }else { 
      obsr=end(gdpts)[2]+1
      year=end(gdpts)[1]
      
      }
    
    fcasts=ts(fcasts, start=c(year,obsr), frequency=52)
    
    dates=ts(as.character(data[,2]), start=c(2011,50), frequency=52)
    fit=round(fit)
    fcasts=round(fcasts)
    asdf=cbind(dates,salests,fit,fcasts)
    asdf=data.frame(asdf)
    names(asdf)=c("Period","Actual", "Fitted","Forecast")
    return(asdf)
    
    
    
    
  })
  
  
  output$dlmforecastplot=renderPlot({
    #DYNAMIC MODELS      
    if(is.null(TheTS())) { return(NULL)}
    
    data=Dataset()
    salesindex=as.numeric(grep(as.character(input$dlmindex), colnames(data))) #index for sales/actuals
    selecteddate=as.character(input$num029observ)
    daterowindex=grep(selecteddate,data[,grep("Period",colnames(data))])
    
    
    
    sales=data[,salesindex]
    sales=head(sales,as.numeric(daterowindex))
    salests=ts(sales, start=c(2011,50), frequency=52) 
    
    price=data[,(salesindex+2)]
    price=head(price,as.numeric(daterowindex))
    pricets=ts(price, start=c(2011,50), frequency=52)
    
    gdpindex=as.numeric(grep("GDP",colnames(data)))
    gdp=data[,gdpindex]
    gdp=head(gdp,as.numeric(daterowindex))
    gdpts=ts(gdp, start=c(2011,50), frequency=52)      
    
    m1=dynlm(salests~pricets+gdpts+L(salests, 1) + L(salests, 2)) #model
    
    
    
    fcasts=c()
    for(iter in (as.numeric(daterowindex)+1):nrow(data)){ 
      #intercept, price, gdp, lag1, lag 2
      value=coef(m1) %*% c(1, (data[iter,(salesindex+2)]), data[iter,gdpindex],data[(iter-1),salesindex],data[(iter-2),salesindex])
      fcasts=c(fcasts,value)
    }
    
    #figure out where to start forcasts as time series
    if(end(gdpts)[2]==52){
      obsr=1
      year=end(gdpts)[1]+1
      
    }else { 
      obsr=end(gdpts)[2]+1
      year=end(gdpts)[1]
      
    }
    
    fcasts=ts(fcasts, start=c(year,obsr), frequency=52)
    return ( plot(fcasts, main=paste("Forecast with Price Multiplier= 1"), col="purple" ,ylab="Sales=f(price,gdp,t-1, t-2)"))
    
  })
  
  
  output$dlmsimulatedplot=renderPlot({ 

    #forecast, simulate price
    #DYNAMIC MODELS      
    if(is.null(TheTS())) { return(NULL)}
    
    data=Dataset()
    salesindex=as.numeric(grep(as.character(input$dlmindex), colnames(data))) #index for sales/actuals
    selecteddate=as.character(input$num029observ)
    daterowindex=grep(selecteddate,data[,grep("Period",colnames(data))]) #NUMBER OF OBSERVATIONS IN REGRESSION
    
    
    
    sales=data[,salesindex]
    sales=head(sales,as.numeric(daterowindex))
    salests=ts(sales, start=c(2011,50), frequency=52) 
    
    price=data[,(salesindex+2)]
    price=head(price,as.numeric(daterowindex))
    pricets=ts(price, start=c(2011,50), frequency=52)
    
    gdpindex=as.numeric(grep("GDP",colnames(data)))
    gdp=data[,gdpindex]
    gdp=head(gdp,as.numeric(daterowindex))
    gdpts=ts(gdp, start=c(2011,50), frequency=52)      
    
    m1=dynlm(salests~pricets+gdpts+L(salests, 1) + L(salests, 2)) #model
    
    
    
    fcasts=c()
    for(iter in (as.numeric(daterowindex)+1):nrow(data)){ 
      #intercept, price, gdp, lag1, lag 2
      value=coef(m1) %*% c(1, ((input$priceslider)*(data[iter,(salesindex+2)])), data[iter,gdpindex],data[(iter-1),salesindex],data[(iter-2),salesindex])
      fcasts=c(fcasts,value)
    }
    
    #figure out where to start forcasts as time series
    if(end(gdpts)[2]==52){
      obsr=1
      year=end(gdpts)[1]+1
      
    }else { 
      obsr=end(gdpts)[2]+1
      year=end(gdpts)[1]
      
    }
    
    fcasts=ts(fcasts, start=c(year,obsr), frequency=52)
    
    return ( plot(fcasts, main=paste("Forecast with Price Multiplier=", input$priceslider, sep=""), col="purple" ,ylab="Sales=f(price,gdp,t-1, t-2)"))
    
    
  })
  
  output$diff029plot=renderPlot({
    
    
    #DYNAMIC MODELS      
    if(is.null(TheTS())) { return(NULL)}
    
    data=Dataset()
    salesindex=as.numeric(grep(as.character(input$dlmindex), colnames(data))) #index for sales/actuals
    selecteddate=as.character(input$num029observ)
    daterowindex=grep(selecteddate,data[,grep("Period",colnames(data))])
    
    
    
    sales=data[,salesindex]
    sales=head(sales,as.numeric(daterowindex))
    salests=ts(sales, start=c(2011,50), frequency=52) 
    
    price=data[,(salesindex+2)]
    price=head(price,as.numeric(daterowindex))
    pricets=ts(price, start=c(2011,50), frequency=52)
    
    gdpindex=as.numeric(grep("GDP",colnames(data)))
    gdp=data[,gdpindex]
    gdp=head(gdp,as.numeric(daterowindex))
    gdpts=ts(gdp, start=c(2011,50), frequency=52)      
    
    m1=dynlm(salests~pricets+gdpts+L(salests, 1) + L(salests, 2)) #model

    #figure out where to start forcasts as time series
    if(end(gdpts)[2]==52){
      obsr=1
      year=end(gdpts)[1]+1
      
    }else { 
      obsr=end(gdpts)[2]+1
      year=end(gdpts)[1]
      
    }
    
    
    
    fixedfcasts=c()
    for(iter in (as.numeric(daterowindex)+1):nrow(data)){ 
      #intercept, price, gdp, lag1, lag 2
      value=coef(m1) %*% c(1, (data[iter,(salesindex+2)]), data[iter,gdpindex],data[(iter-1),salesindex],data[(iter-2),salesindex])
      fixedfcasts=c(fixedfcasts,value)
    }
    
    
    fixedfcasts=ts(fixedfcasts, start=c(year,obsr), frequency=52)
    
    simfcasts=c()
    for(iter in (as.numeric(daterowindex)+1):nrow(data)){ 
      #intercept, price, gdp, lag1, lag 2
      value=coef(m1) %*% c(1, ((input$priceslider)*(data[iter,(salesindex+2)])), data[iter,gdpindex],data[(iter-1),salesindex],data[(iter-2),salesindex])
      simfcasts=c(simfcasts,value)
    }
    
    simfcasts=ts(simfcasts, start=c(year,obsr), frequency=52)
    
    differencefcasts=simfcasts-fixedfcasts
    differencefcasts=ts(differencefcasts, start=c(year,obsr), frequency=52)
    plot(differencefcasts, main="Difference in Fixed and Simulated Forecasts")
    
    
    
    
  })
  

  
  
 
  
  
  
# 031 simulation   

  
  output$dlm2Plot=renderPlot({
    #actuals vs fitted
    #DYNAMIC MODELS      
    if(is.null(TheTS())) { return(NULL)}
    
    data=Dataset()
    
    if(input$unitselected==1){
      depvarindex=as.numeric(grep("A_31BUV",colnames(data)))
      priceindex=as.numeric(grep("L31BPU",colnames(data)))
      natpriceindex=as.numeric(grep("L29BPU",colnames(data)))
      
    }else{
      depvarindex=as.numeric(grep("A_L31BT",colnames(data)))
      priceindex=as.numeric(grep("L31BPT",colnames(data)))
      natpriceindex=as.numeric(grep("L29BPT",colnames(data)))
    }
    selecteddate=as.character(input$num031observ)
    daterowindex=grep(selecteddate,data[,grep("Period",colnames(data))])
    
    numobservations=as.numeric(daterowindex)
    actual=data[,depvarindex]
    actual=head(actual,numobservations)
    actualts=ts(actual, start=c(2011,50), frequency=52)
    
    gdpindex=as.numeric(grep("GDP",colnames(data)))
    gdp=data[,gdpindex]
    gdp=head(gdp,numobservations)
    gdpts=ts(gdp, start=c(2011,50), frequency=52)
    
    price=data[,priceindex]
    price=head(price,numobservations)
    pricets=ts(price, start=c(2011,50), frequency=52) 
    
    natprice=data[,natpriceindex]
    natprice=head(natprice,numobservations)
    natpricets=ts(natprice, start=c(2011,50), frequency=52) 
    
    
    m5=dynlm(actualts~gdpts+pricets+natpricets+L(actualts, 1) + L(actualts, 2)) # unit model
    
    fit=predict(m5)   
    fit=ts(fit,start=c(2011,52), frequency = 52)
    plot(fit, type="l", col="red", ylab="Sales=f(GDP, 31Price, 29price, L-1, L-2)", main="Actuals and Model Fitted Values") #fitted
    lines(actualts, col="blue") #actuals
    legend("top", c("Actuals","Fitted"), lty=c(1,1),  lwd=c(2.5,2.5) ,col=c("blue","red"))
    
  })
  
  output$dlm2FCPlot=renderPlot({
    
    #actuals vs fitted
    #DYNAMIC MODELS      
    if(is.null(TheTS())) { return(NULL)}
    
    data=Dataset()
    
    if(input$unitselected==1){
      depvarindex=as.numeric(grep("A_31BUV",colnames(data)))
      priceindex=as.numeric(grep("L31BPU",colnames(data)))
      natpriceindex=as.numeric(grep("L29BPU",colnames(data)))
      
    }else{
      depvarindex=as.numeric(grep("A_L31BT",colnames(data)))
      priceindex=as.numeric(grep("L31BPT",colnames(data)))
      natpriceindex=as.numeric(grep("L29BPT",colnames(data)))
    }
    
    selecteddate=as.character(input$num031observ)
    daterowindex=grep(selecteddate,data[,grep("Period",colnames(data))])
    
    numobservations=as.numeric(daterowindex)

    actual=data[,depvarindex]
    actual=head(actual,numobservations)
    actualts=ts(actual, start=c(2011,50), frequency=52)
    
    gdpindex=as.numeric(grep("GDP",colnames(data)))
    gdp=data[,gdpindex]
    gdp=head(gdp,numobservations)
    gdpts=ts(gdp, start=c(2011,50), frequency=52)
    
    price=data[,priceindex]
    price=head(price,numobservations)
    pricets=ts(price, start=c(2011,50), frequency=52) 
    
    natprice=data[,natpriceindex]
    natprice=head(natprice,numobservations)
    natpricets=ts(natprice, start=c(2011,50), frequency=52) 
    
    
    m5=dynlm(actualts~gdpts+pricets+natpricets+L(actualts, 1) + L(actualts, 2)) # unit model
    
    
    fcasts=c()
    for(iter in (numobservations+1):nrow(data)){ 
      #intercept,  gdp,price index, natprice index, lag1, lag 2
      value=coef(m5) %*% c(1, data[iter,gdpindex], ((input$priceslider31)*(data[iter,priceindex])),((input$priceslider29)*(data[iter,natpriceindex])), data[(iter-1),depvarindex],data[(iter-2),depvarindex])
      fcasts=c(fcasts,value)
    }
    if(end(gdpts)[2]==52){
      obsr=1
      year=end(gdpts)[1]+1
      
    }else { 
      obsr=end(gdpts)[2]+1
      year=end(gdpts)[1]
      
    }
    
    
    
    fcasts=ts(fcasts, start=c(year,obsr), frequency=52)
    plot(fcasts, main=paste("Forecast with Price Multiplier 31:", input$priceslider31, "Price Multiplier 029:", input$priceslider29, sep=" "), ylab="Sales=f(GDP, 31Price, 29price, L-1, L-2)", col="purple")
    

    
    
    
  })
  
  output$dlm2FixedFCPlot=renderPlot({
    
    #actuals vs fitted
    #DYNAMIC MODELS      
    if(is.null(TheTS())) { return(NULL)}
    
    data=Dataset()
    
    if(input$unitselected==1){
      depvarindex=as.numeric(grep("A_31BUV",colnames(data)))
      priceindex=as.numeric(grep("L31BPU",colnames(data)))
      natpriceindex=as.numeric(grep("L29BPU",colnames(data)))
      
    }else{
      depvarindex=as.numeric(grep("A_L31BT",colnames(data)))
      priceindex=as.numeric(grep("L31BPT",colnames(data)))
      natpriceindex=as.numeric(grep("L29BPT",colnames(data)))
    }
    
    selecteddate=as.character(input$num031observ)
    daterowindex=grep(selecteddate,data[,grep("Period",colnames(data))])
    
    numobservations=as.numeric(daterowindex)
    
    
    actual=data[,depvarindex]
    actual=head(actual,numobservations)
    actualts=ts(actual, start=c(2011,50), frequency=52)
    
    gdpindex=as.numeric(grep("GDP",colnames(data)))
    gdp=data[,gdpindex]
    gdp=head(gdp,numobservations)
    gdpts=ts(gdp, start=c(2011,50), frequency=52)
    
    price=data[,priceindex]
    price=head(price,numobservations)
    pricets=ts(price, start=c(2011,50), frequency=52) 
    
    natprice=data[,natpriceindex]
    natprice=head(natprice,numobservations)
    natpricets=ts(natprice, start=c(2011,50), frequency=52) 
    
    
    m5=dynlm(actualts~gdpts+pricets+natpricets+L(actualts, 1) + L(actualts, 2)) # unit model
    
    
    fcasts=c()
    for(iter in (numobservations+1):nrow(data)){ 
      #intercept,  gdp,price index, natprice index, lag1, lag 2
      value=coef(m5) %*% c(1, data[iter,gdpindex], ((1)*(data[iter,priceindex])),((1)*(data[iter,natpriceindex])), data[(iter-1),depvarindex],data[(iter-2),depvarindex])
      fcasts=c(fcasts,value)
    }
    if(end(gdpts)[2]==52){
      obsr=1
      year=end(gdpts)[1]+1
      
    }else { 
      obsr=end(gdpts)[2]+1
      year=end(gdpts)[1]
      
    }
    
    
    
    fcasts=ts(fcasts, start=c(year,obsr), frequency=52)
    plot(fcasts, main="Forecast with Price Multipliers =1", col="purple")
    
    
    
    
    
  })
  
  output$dlm2FCdiffPlot=renderPlot({
    
    #actuals vs fitted
    #DYNAMIC MODELS      
    if(is.null(TheTS())) { return(NULL)}
    
    data=Dataset()
    
    if(input$unitselected==1){
      depvarindex=as.numeric(grep("A_31BUV",colnames(data)))
      priceindex=as.numeric(grep("L31BPU",colnames(data)))
      natpriceindex=as.numeric(grep("L29BPU",colnames(data)))
      
    }else{
      depvarindex=as.numeric(grep("A_L31BT",colnames(data)))
      priceindex=as.numeric(grep("L31BPT",colnames(data)))
      natpriceindex=as.numeric(grep("L29BPT",colnames(data)))
    }
    
    selecteddate=as.character(input$num031observ)
    daterowindex=grep(selecteddate,data[,grep("Period",colnames(data))])
    
    numobservations=as.numeric(daterowindex)
    actual=data[,depvarindex]
    actual=head(actual,numobservations)
    actualts=ts(actual, start=c(2011,50), frequency=52)
    
    gdpindex=as.numeric(grep("GDP",colnames(data)))
    gdp=data[,gdpindex]
    gdp=head(gdp,numobservations)
    gdpts=ts(gdp, start=c(2011,50), frequency=52)
    
    price=data[,priceindex]
    price=head(price,numobservations)
    pricets=ts(price, start=c(2011,50), frequency=52) 
    
    natprice=data[,natpriceindex]
    natprice=head(natprice,numobservations)
    natpricets=ts(natprice, start=c(2011,50), frequency=52) 
    
    
    m5=dynlm(actualts~gdpts+pricets+natpricets+L(actualts, 1) + L(actualts, 2)) # unit model
    
    
    #fixed
    fixedfcasts=c()
    for(iter in (numobservations+1):nrow(data)){ 
      #intercept,  gdp,price index, natprice index, lag1, lag 2
      value=coef(m5) %*% c(1, data[iter,gdpindex], ((1)*(data[iter,priceindex])),((1)*(data[iter,natpriceindex])), data[(iter-1),depvarindex],data[(iter-2),depvarindex])
      fixedfcasts=c(fixedfcasts,value)
    }
    if(end(gdpts)[2]==52){
      obsr=1
      year=end(gdpts)[1]+1
      
    }else { 
      obsr=end(gdpts)[2]+1
      year=end(gdpts)[1]
      
    }

    fixedfcasts=ts(fixedfcasts, start=c(year,obsr), frequency=52)

    
    
    
    simfcasts=c()
    for(iter in (numobservations+1):nrow(data)){ 
      #intercept,  gdp,price index, natprice index, lag1, lag 2
      value=coef(m5) %*% c(1, data[iter,gdpindex], ((input$priceslider31)*(data[iter,priceindex])),((input$priceslider29)*(data[iter,natpriceindex])), data[(iter-1),depvarindex],data[(iter-2),depvarindex])
      simfcasts=c(simfcasts,value)
    }
    simfcasts=ts(simfcasts, start=c(year,obsr), frequency=52)
    
    differencefcasts=simfcasts-fixedfcasts
    differencefcasts=ts(differencefcasts, start=c(year,obsr), frequency=52)
    plot(differencefcasts, main="Difference in Fixed and Simulated Forecasts")
    
    
    
    
    
  })
  
  output$dlm2Table=renderTable({
    
    #actuals vs fitted
    #DYNAMIC MODELS      
    if(is.null(TheTS())) { return(NULL)}
    
    data=Dataset()
    
    if(input$unitselected==1){
      depvarindex=as.numeric(grep("A_31BUV",colnames(data)))
      priceindex=as.numeric(grep("L31BPU",colnames(data)))
      natpriceindex=as.numeric(grep("L29BPU",colnames(data)))
      
    }else{
      depvarindex=as.numeric(grep("A_L31BT",colnames(data)))
      priceindex=as.numeric(grep("L31BPT",colnames(data)))
      natpriceindex=as.numeric(grep("L29BPT",colnames(data)))
    }
    selecteddate=as.character(input$num031observ)
    daterowindex=grep(selecteddate,data[,grep("Period",colnames(data))])
    
    numobservations=as.numeric(daterowindex)
    actual=data[,depvarindex]
    actual=head(actual,numobservations)
    actualts=ts(actual, start=c(2011,50), frequency=52)
    
    gdpindex=as.numeric(grep("GDP",colnames(data)))
    gdp=data[,gdpindex]
    gdp=head(gdp,numobservations)
    gdpts=ts(gdp, start=c(2011,50), frequency=52)
    
    price=data[,priceindex]
    price=head(price,numobservations)
    pricets=ts(price, start=c(2011,50), frequency=52) 
    
    natprice=data[,natpriceindex]
    natprice=head(natprice,numobservations)
    natpricets=ts(natprice, start=c(2011,50), frequency=52) 
    
    
    m5=dynlm(actualts~gdpts+pricets+natpricets+L(actualts, 1) + L(actualts, 2)) # unit model
    
    fit=predict(m5)   
    fit=ts(fit,start=c(2011,52), frequency = 52)
    
    fcasts=c()
    for(iter in (numobservations+1):nrow(data)){ 
      #intercept,  gdp,price index, natprice index, lag1, lag 2
      value=coef(m5) %*% c(1, data[iter,gdpindex], ((input$priceslider31)*(data[iter,priceindex])),((input$priceslider29)*(data[iter,natpriceindex])), data[(iter-1),depvarindex],data[(iter-2),depvarindex])
      fcasts=c(fcasts,value)
    }
    if(end(gdpts)[2]==52){
      obsr=1
      year=end(gdpts)[1]+1
      
    }else { 
      obsr=end(gdpts)[2]+1
      year=end(gdpts)[1]
      
    }

    fcasts=ts(fcasts, start=c(year,obsr), frequency=52)
    
    dates=ts(as.character(data[,2]), start=c(2011,50), frequency=52)
    fit=round(fit)
    fcasts=round(fcasts)
    asdf=cbind(dates,actualts,fit,fcasts)
    asdf=data.frame(asdf)
    names(asdf)=c("Period","Actual", "Fitted","Forecast")
    return(asdf)
    
    
    
  })
  
  
  
  
  
  
  
  
  
  #decomp and residuals
  output$decompPlot=renderPlot({
    if(is.null(TheTS())) { return(NULL)}
    TS=TheTS()
    
    ListD=stl(TS, s.window="periodic", robust=input$robust)
    plot(ListD,main=paste(input$vars, "Decomposition", sep=" "))
    
  })
  
  
  
  output$residualPlot=renderPlot({
    if(is.null(TheTS())) { return(NULL)}
    act=TheTS() #actuals
    fit=TheTS1()#fitted
    plot((act-fit), main="Residuals vs Time", ylab="Residuals", xlab="Time", type=input$resplotType)
    
  })
  
  output$acfPlot=renderPlot({
    if(is.null(TheTS())) { return(NULL)}
    act=TheTS() #actuals
    fit=TheTS1()#fitted
    Acf((act-fit), lag=52 , main="Autocorrelation Function of residuals")
    
  })

  
  
  
    
  output$wf031=renderPlot({
    
    
    #waterfall plot with original regression coefficients
    
    if (is.null(input$file1)) { return (NULL) }
    
    interventions=invs()
    
    depvarindex=as.numeric(grep("A_L31BT", colnames(interventions)))
    dependent=interventions[,depvarindex]
    dependent=ts(dependent,start=c(2011,50) , frequency=52)
    
    variables=list()
    for(i in 1:ncol(interventions)){
      
      if(i==depvarindex){ next }
      curr=interventions[,i]
      currts=ts(curr , start=c(2011,50) , frequency=52)
      variables=append(variables, list(currts), after=length(variables))
      
      
    }
    
    rhs=""
    for(i in 1:length(variables)){
      rhs=paste( rhs, "+variables[[", as.character(i), "]]", sep="")
      
    }
    asdf=as.formula(paste("dependent~L(dependent,1)+L(dependent,2)", as.character(rhs)))
    automodel=dynlm(asdf)
    coef=automodel$coefficients
    names(coef)=c("Intercept","lag1","lag2","GDP","L29BPT","L31BPT","Pulse4","SP119","SP120","SP121","Pulse145","Pulse146","Pulse148")
    
    
    
    #waterfall chart method 1--> orignial coef*mean(variable)
    effectsof=c("GDP","L29BPT","L31BPT","Pulse4" ,"SP119","SP120","SP121","Pulse145","Pulse146","Pulse148") #names of whatever wanted
    start=c(0)
    end=c(coef["Intercept"])
    changes=c(coef["Intercept"])
    type=c("Intercept")
    for(i in 1:length(effectsof)){
      
      newstart=as.numeric(end[length(end)])
      start=c(start,newstart)
      
      
      avg=mean(interventions[,grep(effectsof[i], colnames(interventions))])
      
      coefindex=grep(effectsof[i], names(coef))
      delta=coef[coefindex]*avg
      if(delta>0){ type=c(type,"Gain")} else {type=c(type,"Loss")}
      
      changes=c(changes,delta)
      
      end=c(end,(newstart+delta))
      
    }
    
    wfdf=data.frame(id=1:length(start),start,end,changes,type)
    .e <- environment()
    
    
    ggplot(wfdf, aes(c("",effectsof),fill=type), environment=.e ) +
     geom_rect(aes(x=c("",effectsof), xmin=id-0.45,xmax=id+0.45,ymin=end,ymax=start ))+
    scale_fill_manual(values=c("Gain"="blue","Intercept"="black","Loss"="Red"))+ylab("Change")+xlab("Variable")+ggtitle("Key Driver Contribution") +
     geom_text( aes(id, end, label =round(end) ), vjust = ifelse(end < start, 1, -0.3), size = 4) 
    
    
    
    
    })
  
  
  output$bwwf031=renderPlot({
    #waterfall with betaweights
    
    if (is.null(input$file1)) { return (NULL) }
    
    interventions=invs()
    
    depvarindex=as.numeric(grep("A_L31BT", colnames(interventions)))
    dependent=interventions[,depvarindex]
    dependent=ts(dependent,start=c(2011,50) , frequency=52)
    
    variables=list()
    for(i in 1:ncol(interventions)){
      
      if(i==depvarindex){ next }
      curr=interventions[,i]
      currts=ts(curr , start=c(2011,50) , frequency=52)
      variables=append(variables, list(currts), after=length(variables))
      
      
    }
    
    rhs=""
    for(i in 1:length(variables)){
      rhs=paste( rhs, "+variables[[", as.character(i), "]]", sep="")
      
    }
    asdf=as.formula(paste("dependent~L(dependent,1)+L(dependent,2)", as.character(rhs)))
    automodel=dynlm(asdf)
    coef=automodel$coefficients
    names(coef)=c("Intercept","lag1","lag2","GDP","L29BPT","L31BPT","Pulse4","SP119","SP120","SP121","Pulse145","Pulse146","Pulse148")
    
    
    betaweights=c()
    for(i in 1:length(coef)){
      i=as.numeric(i)
      sy=sd(interventions[,depvarindex])
      
      if(names(coef)[i]=="lag1"||names(coef)[i]=="lag2"||names(coef)[i]=="Intercept"){  next  }
      else{
        
        bi=coef[i]
        colindex=grep(names(coef)[i], colnames(interventions))
        si=sd(interventions[,colindex])
        BI=bi*(si/sy)
        betaweights=c(betaweights,BI)
      }
    }
    
    
    effectsof=c("GDP","L29BPT","L31BPT","Pulse4" ,"SP119","SP120","SP121","Pulse145","Pulse146","Pulse148") #names of whatever wanted
    start=c(0)
    end=c(1)
    changes=c(1)
    type=c("Intercept")
    for(i in 1:length(effectsof)){
      
      newstart=as.numeric(end[length(end)])
      start=c(start,newstart)
      
      
      
      bwindex=grep(effectsof[i], names(betaweights))
      delta=betaweights[bwindex]*1
      if(delta>0){ type=c(type,"Gain")} else {type=c(type,"Loss")}
      
      changes=c(changes,delta)
      
      end=c(end,(newstart+delta))
      
    }
    
    wfdf=data.frame(id=1:length(start),start,end,changes,type)
    
    .e <- environment()
    ggplot(wfdf, aes(c("",effectsof),fill=type), environment=.e ) + 
    geom_rect(aes(x=c("",effectsof), xmin=id-0.45,xmax=id+0.45,ymin=end,ymax=start ))+ylab("Change")+xlab("Variable")+
    ggtitle("Key Driver Contribution (Normalized)")+scale_fill_manual(values=c("Gain"="blue","Intercept"="black","Loss"="Red"))+
    geom_text( aes(id, end, label =round(end,3) ), vjust = ifelse(end < start, 1, -0.3), size = 4)
    
    
  })
  
  
  output$wf029=renderPlot({
    #waterfall plot with original regression coefficients
    
    if (is.null(input$file1)) { return (NULL) }
    
    interventions=invs2()
    
    names(interventions)=c("GDP"    ,  "L29BPT",   "Pulse145" ,"A_L3029BT" )
    coef=c(163299,0.0319074,-18812.9,57798.7)
    names(coef)=c("Intercept","GDP","L29BPT","Pulse145")
    

    effectsof=c("GDP","L29BPT","Pulse145") #names of whatever wanted
    start=c(0)
    end=c(coef["Intercept"])
    changes=c(coef["Intercept"])
    type=c("Intercept")
    for(i in 1:length(effectsof)){
      
      newstart=as.numeric(end[length(end)])
      start=c(start,newstart)
      
      
      avg=mean(interventions[,grep(effectsof[i], colnames(interventions))])
      
      coefindex=grep(effectsof[i], names(coef))
      delta=coef[coefindex]*avg
      if(delta>0){ type=c(type,"Gain")} else {type=c(type,"Loss")}
      
      changes=c(changes,delta)
      
      end=c(end,(newstart+delta))
      
    }
    
    wfdf=data.frame(id=1:length(start),start,end,changes,type)
    .e <- environment()
    ggplot(wfdf, aes(c("",effectsof),fill=type), environment=.e ) + 
    geom_rect(aes(x=c("",effectsof), xmin=id-0.45,xmax=id+0.45,ymin=end,ymax=start ))+
    scale_fill_manual(values=c("Gain"="blue","Intercept"="black","Loss"="Red"))+ylab("Change")+xlab("Variable")+ggtitle("Key Driver Contribution")+
    geom_text( aes(id, end, label =round(end) ), vjust = ifelse(end < start, 1, -0.3), size = 4)
    

    
    
      })
  
  
  output$bwwf029=renderPlot({
    if (is.null(input$file1)) { return (NULL) }
    
    interventions=invs2()
    
    names(interventions)=c("GDP"    ,  "L29BPT",   "Pulse145" ,"A_L3029BT" )
    coef=c(163299,0.0319074,-18812.9,57798.7)
    names(coef)=c("Intercept","GDP","L29BPT","Pulse145")
    
    
    

    betaweights=c()
    depvarindex=as.numeric(grep("A_L3029BT", colnames(interventions)))
    for(i in 1:length(coef)){
      i=as.numeric(i)
      sy=sd(interventions[,depvarindex])
      
      if(names(coef)[i]=="lag1"||names(coef)[i]=="lag2"||names(coef)[i]=="Intercept"){  next  }
      else{
        
        bi=coef[i]
        colindex=grep(names(coef)[i], colnames(interventions))
        si=sd(interventions[,colindex])
        BI=bi*(si/sy)
        betaweights=c(betaweights,BI)
      }
    }
    
    
    effectsof=c("GDP","L29BPT","Pulse145") #names of whatever wanted
    start=c(0)
    end=c(1)
    changes=c(1)
    type=c("Intercept")
    for(i in 1:length(effectsof)){
      
      newstart=as.numeric(end[length(end)])
      start=c(start,newstart)
      
      
      
      bwindex=grep(effectsof[i], names(betaweights))
      delta=betaweights[bwindex]*1
      if(delta>0){ type=c(type,"Gain")} else {type=c(type,"Loss")}
      
      changes=c(changes,delta)
      
      end=c(end,(newstart+delta))
      
    }
    
    wfdf=data.frame(id=1:length(start),start,end,changes,type)
    .e <- environment()
    ggplot(wfdf, aes(c("",effectsof),fill=type) , environment=.e) + 
    geom_rect(aes(x=c("",effectsof), xmin=id-0.45,xmax=id+0.45,ymin=end,ymax=start ))+
    scale_fill_manual(values=c("Gain"="blue","Intercept"="black","Loss"="Red"))+ylab("Change")+xlab("Variable")+ggtitle("Key Driver Contribution (Normalized)")+
    geom_text( aes(id, end, label =round(end,3) ), vjust = ifelse(end < start, 1, -0.3), size = 4)
    
    
    
  })
  
  
  
  
  GoogleDataTrend <- reactive({
    query=input$query
    if (is.null(query)) {
      return(NULL)
    }
    
    usr <- "ASAdvancedAnalytics@gmail.com"
    psw <- "Cre@tiv3"
    
    ch <- gconnect(usr, psw)
    
    
    data <- gtrends(ch, query)
    googd=data$trend
    googd
    
  })
  
  
  
  output$trendplot=renderPlot({
    query=input$query
    if (is.null(query)) {
      return(NULL)
    }
    x=GoogleDataTrend()
    #return indexes for start and end dates
    date1=as.character(input$date1)
    date2=as.character(input$date2)
    
    ind1=grep(date1,x$start)
    ind2=grep(date2,x$start)
    
    #subset data frame, use these for graph
    xsub=x[ind1:ind2,]
    
    plot(xsub$start, xsub[,3], type=input$googleplotType, xlab="Time", ylab="Interest", main= paste(input$query, "Interest via Google Trends", input$date1, "to", input$date2, sep=" "), col="green")  #need type to be input by user
    
  })
  
  
  
  GoogleDataTopSearches <- reactive({
    query=input$query
    if (is.null(query)) {
      return(NULL)
    }
    
    usr <- "ASAdvancedAnalytics@gmail.com"
    psw <- "Cre@tiv3"
    
    ch <- gconnect(usr, psw)
    
    
    data <- gtrends(ch, query)
    googd=as.data.frame(data$searches)
    googd
    
  })
  
  output$searchWordCloud <- renderPlot({
    
    query=input$query
    if (is.null(query)) {
      return(NULL)
    }
    searchdf=GoogleDataTopSearches()
    wordcloud(searchdf[,1], searchdf[,2],scale=c(5,.5), colors=brewer.pal(9, "Set1"))
    
  })
  
  

  
  
})

