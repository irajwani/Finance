#install.packages(c("shiny","shinythemes","ggplot2","pROC","xts","quantmod","PerformanceAnalytics","quantmod","dygraphs","httr","jsonlite","rjson","devtools","Rforecastio","darksky","placement"))
require(shiny)
require(shinythemes)
require(ggplot2)
require(pROC)
require(xts)
require(PerformanceAnalytics)
require(quantmod)
require(dygraphs)
require(httr)
require(jsonlite)
require(rjson)
require(devtools)
require(darksky)
require(plyr)
require(RCurl)
require(placement)
#require(RCurl)
#api key for darksky: 4808d4342a23b2fab0c9f632bdf2c179
#api key for reddit: 48891390a8d14f119d340e4830c3f1ed
shinyServer(function(input,output) {
  
  news <- function(publication,sort){
    url<-paste('https://newsapi.org/v1/articles?source=',publication,'&sortBy=',sort,'&apiKey=48891390a8d14f119d340e4830c3f1ed',sep='')
    raw <- GET(url)
    names(raw)
    raw$status_code
    content(raw)
    head(raw$content)
    neat<-rawToChar(raw$content)
    neat1<-fromJSON(neat)
    print(neat1$articles[[2]])
  }
  
  piot <- function(stock) {
    F=0
    finance<-getFinancials(stock, src = 'yahoo',auto.assign = FALSE)
    revenues<-finance$IS$A[1]
    cogs<-finance$IS$A[3]
    netincome<-finance$CF$A[1]
    totalassets<-finance$BS$A[17]
    ltdebt<-finance$BS$A[26]
    cassets<-finance$BS$A[10]
    cliabilities<-finance$BS$A[23]
    outshares<-finance$BS$A[42]
    prerevenues<-finance$IS$A[1,2]
    precogs<-finance$IS$A[3,2]
    preoutshares<-finance$BS$A[42,2]
    precliabilities<-finance$BS$A[23,2]
    precassets<-finance$BS$A[10,2]
    preltdebt<-finance$BS$A[26,2]
    prenetincome<-finance$CF$A[1,2]
    pretotalassets<-finance$BS$A[17,2]
    netchangeincash<-finance$CF$A[17]
    #Criteria:
    #1. positive return on assets?
    if (netincome/totalassets > 0 ) {F=F+1} else {F=F+0} 
    #2. higher ROA in current year than in previous year?
    if (netincome/totalassets >= prenetincome/pretotalassets) {F=F+1} else {F=F+0} 
    #3. positive operating cash flow?
    if (netchangeincash > 0) {F=F+1} else {F=F+0} 
    #4. Operating Cash flow greater than ROA?
    if(netchangeincash > (netincome/totalassets)) {F=F+1} else {F=F+0}
    #5. Higher LongTermDebt/Total Assets ratio in current year than in previous year?
    if ((ltdebt/totalassets) > (preltdebt/pretotalassets)) {F=F+1} else {F=F+0} 
    #6. Higher current assets/current liabilities ratio in current year than in previous year?
    if ((cassets/cliabilities)>=(precassets/precliabilities) & is.na(cassets)==F) {F=F+1} else {F=F+0} 
    #7. No new shares were issued in the last year?
    if (outshares-preoutshares <= 0) {F=F+1} else {F=F+0}
    #8. A higher gross margin compared to the previous year?
    if ((revenues-cogs)/revenues >= (prerevenues-precogs)/prerevenues) {F=F+1} else{F=F+0}
    #9. A higher asset turnover ratio than in the previous year?
    if (revenues/totalassets > prerevenues/pretotalassets) {F=F+1} else{F=F+0}
    paste(print(F),"is the Piotroski F score for",print(stock), sep=" ")
    
  }
  
  msr <- function(ticker1,ticker2,start_year) {
    
    symbol <- getSymbols(ticker1, src = 'yahoo', auto.assign = FALSE, warnings = FALSE)
    
    # Tranform it to monthly returns using the periodReturn function from quantmod
    prdata1 <- periodReturn(symbol, period = 'monthly', subset=paste(start_year, "::", sep = ""), 
                            type = 'log')
    
    # Let's rename the column of returns to something intuitive because the column name is what
    # will eventually be displayed on the time series graph.
    colnames(prdata1) <- as.character(ticker1)
    # We want to be able to work with the xts objects that result from this function 
    # so let's explicitly put them to the global environment with an easy to use 
    # name, the stock ticker.
    assign(ticker1, prdata1, .GlobalEnv)
    symbol <- getSymbols(ticker2, src = 'yahoo', auto.assign = FALSE, warnings = FALSE)
    
    # Tranform it to monthly returns using the periodReturn function from quantmod
    prdata2 <- periodReturn(symbol, period = 'monthly', subset=paste(start_year, "::", sep = ""), 
                           type = 'log')
    
    # Let's rename the column of returns to something intuitive because the column name is what
    # will eventually be displayed on the time series graph.
    colnames(prdata2) <- as.character(ticker2)
    # We want to be able to work with the xts objects that result from this function 
    # so let's explicitly put them to the global environment with an easy to use 
    # name, the stock ticker.
    assign(ticker2, prdata2, .GlobalEnv)
    
    mergedreturns<-merge.xts(prdata1,prdata2)
    
    dygraph(mergedreturns, main = c(ticker1,ticker2)) %>%
      dyAxis("y", label = "%") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
  }

  
  pv1<-function(ticker1) {
    symbol1 <- getSymbols(ticker1,src='yahoo',auto.assign = FALSE, warnings=FALSE)
    chartSeries(symbol1,type="candlesticks",clev=0,name = colnames(symbol1),TA='addVo()',theme=chartTheme("black",up.col="blue",dn.col="red",multi.col = "red"),line.type="1")
  }
  
  pv2<-function(ticker2) {
  symbol2<- getSymbols(ticker2,src='yahoo',auto.assign = FALSE, warnings=FALSE)
  chartSeries(symbol2,type="candlesticks",clev=0,name =colnames(symbol2),TA='addVo()',theme=chartTheme("black",up.col="blue",dn.col="red",multi.col = "red"),line.type="1")
  }
  quo<-function(ticker1){
    a<-getQuote(ticker1,what=yahooQF("Previous Close"))
    a$`P. Close`
  }
  
  divid<-function(ticker1){
    d<-getDividends(ticker1,src='yahoo',from="2000-01-01",to=Sys.Date(),auto.update = TRUE,auto.assign = FALSE)
    e<-matrix(data=d)
   tail(e,1)
  }
  
  ar<-function(ticker1,start_year){
    symbol <- getSymbols(ticker1, src = 'yahoo', auto.assign = FALSE, warnings = FALSE)
    
    prdata <- periodReturn(symbol, period = 'monthly', subset=paste(start_year, "::", sep = ""), 
                           type = 'log')
    colnames(prdata) <- as.character(ticker1)
    
    Return.annualized(prdata,scale=12,geometric=TRUE)
    
  }
  
  weather_now<-function(lat,lon) {
    
    now<-get_current_forecast(lat,lon,units = "uk", language = "en")
    #current time
    time<-now$currently$time
    #current weather info
    icon<-now$currently$icon
    summ<-now$currently$summary
    #current temp
    temp<-now$currently$temperature
    #current windspeed
    wind<-now$currently$windSpeed
    paste(time,icon,summ,temp,wind, sep=" ")
    
  }
  url <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))
  }
  
  geocode <- function(address,verbose=FALSE) {
    if(verbose) cat(address,"\n")
    u <- url(address)
    doc <- getURL(u)
    #x <- fromJSON(doc,simplify = FALSE)
    x <- fromJSON(doc)
    
    if(x$status=="OK") {
      lat <- x$results[[1]]$geometry$location$lat
      lng <- x$results[[1]]$geometry$location$lng
      location_type <- x$results[[1]]$geometry$location_type
      formatted_address <- x$results[[1]]$formatted_address
      return(c(lat, lng, location_type, formatted_address))
    } else {
      return(c(NA,NA,NA, NA))
    }
  }
  
  # geocodevec<- function(address1,address2,verbose=FALSE){
  #   if(verbose) cat(address1,"\n")
  #   if(verbose) cat(address2,"\n")
  #   a <- c(address1,address2)
  #   locations <- ldply(a, function(x) geocode(x))
  #   names(locations) <- c("lat","lon","location_type", "forAddress")
  #   locations
  #   
  # }
  geocodevec<- function(address1,address2,verbose=FALSE){
    if(verbose) cat(address1,"\n")
    if(verbose) cat(address2,"\n")
    a <- c(address1,address2)
    locations <- ldply(a, function(x) geocode(x))
    names(locations) <- c("lat","lon","location_type", "forAddress")
    locations
    #first places lat:
    # locations[1,1]
    # #first places lon:
    # locations[1,2]
    # #2nd places lat:
    # locations[2,1]
    # #2nd places lon:
    # locations[2,2]
    # start <- paste(locations[1,1],locations[1,2], sep=",")
    # end   <- paste(locations[2,1],locations[2,2], sep=",")
    # howfar_miles <- drive_time(address=start, dest=end, auth="standard_api",
    #                            privkey="", clean=FALSE, add_date='today',
    #                            verbose=FALSE, travel_mode="driving",
    #                            units="metric")
    # howfar_miles
  }
  
  geodistance<-function(address1,address2,travelmode) {
    address <- c(address1,address2)
    coordset <- geocode_url(address, auth="standard_api", privkey="",
                            clean=TRUE, add_date='today', verbose=TRUE)
    #print(coordset[ , 1:5])
    start <- paste(coordset$lat[1],coordset$lng[1], sep=",")
    end   <- paste(coordset$lat[2],coordset$lng[2], sep=",")
    howfar_miles <- drive_time(address=start, dest=end, auth="standard_api",
                               privkey="", clean=FALSE, add_date='today',
                               verbose=FALSE, travel_mode=travelmode,
                               units="metric")
    howfar_miles
  }
  
  weather_hrly<-function(lat,lon) {
    yr<-'2017'
    month<-'07'
    day<-'26'
    hrly<-get_current_forecast(lat,lon,units = 'uk',timestamp = paste(yr,"-",month,"-",day,"T12:00:00-0400",sep = ''),language = "en")
    plot(hrly$hourly$temperature) 
    
  }
  
  output$news<-renderPrint({news(input$publication,input$sort)})
  
  output$map<-renderTable({geocodevec(input$address1,input$address2)})
  output$mapdistance<-renderTable({geodistance(input$address1,input$address2,input$travelmode)})
  
  output$temp <- renderText({weather_now(input$lat,input$lon)})
  output$tempchart <- renderPlot({weather_hrly(input$lat,input$lon)})
  
  output$piotroski <- renderText({piot(input$stock)})
  
  output$result<-renderText({input$budget/quo(input$ticker1)})
  output$resultdesc<-renderText("The number of shares you can buy:")
  
  output$prices<-renderDygraph({msr(input$ticker1,input$ticker2,input$start_year)})
  output$pricesdesc<-renderText("Future value of Dollars invested in Exchange Traded Funds:")
  output$ar<-renderText({(input$budget)*(1+ar(input$ticker1,input$start_year))})
  output$chart1<-renderPlot(pv1(input$ticker1))
  output$chart2<-renderPlot(pv2(input$ticker2))
  output$div<-renderText({input$budget*divid(input$ticker1)*4})
  output$divdesc<-renderText("Dollars you will receive as annual dividends:")
  # output$bean<- renderText({prodprice(input$prodid)})
  # output$pic<- renderText({c('<img src="',prodimg(input$prodid),'">')})
  # output$inv1<-renderText({beaninv1(input$itemid1)})
  # output$bean1<-renderText({prodprice1(input$prodid1)})
  # output$bean2<-renderText({prodprice2(input$prodid2)})
  # output$bean3<-renderText({prodprice3(input$prodid3)})
  # output$bean4<-renderText({prodprice4(input$prodid4)})
  # output$pic1<-renderText({c('<img src="',prodimg1(input$prodid1),'">')}) 
  # output$pic2<-renderText({c('<img src="',prodimg2(input$prodid2),'">')})
  # output$pic3<-renderText({c('<img src="',prodimg3(input$prodid3),'">')})
  # output$pic4<-renderText({c('<img src="',prodimg4(input$prodid4),'">')})

}
)  # beaninv1<-function(itemid1){
#   u<-"https://test.api.llbean.com/v1/inventory/item/"
#   p1<-itemid1
#   e<-"?inventoryLocationId=60%2C40"
#   product<-paste(u,p1,e,sep="")
#   raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
#   names(raw)
#   raw$status_code
#   content(raw)
#   head(raw$content)
#   neat<-rawToChar(raw$content)
#   substr(neat,1,100)
#   neat1<-fromJSON(neat)
#   neat1$properties$items$availability
# }
# prodprice<-function(prodid) {
#   
#   u<-"https://api.llbean.com/v1/products/"
#   p1<-prodid
#   e<-"?expand=images,items,prices"
#   
#   product<-paste(u,p1,e,sep="")
#   
#   raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
#   names(raw)
#   raw$status_code
#   content(raw)
#   head(raw$content)
#   neat<-rawToChar(raw$content)
#   neat1<-fromJSON(neat)
#   neat1$items[[nzchar(neat1$items)]]$prices[[1]]$price
# }
# 
# 
# prodprice1<-function(prodid1) {
#   
#   u<-"https://api.llbean.com/v1/products/"
#   p1<-prodid1
#   e<-"?expand=images,items,prices"
#   
#   product<-paste(u,p1,e,sep="")
#   
#   raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
#   names(raw)
#   raw$status_code
#   content(raw)
#   head(raw$content)
#   neat<-rawToChar(raw$content)
#   neat1<-fromJSON(neat)
#   neat1$items[[nzchar(neat1$items)]]$prices[[1]]$price
# }
# prodprice2<-function(prodid2) {
#   
#   u<-"https://api.llbean.com/v1/products/"
#   p2<-prodid2
#   e<-"?expand=images,items,prices"
#   
#   product<-paste(u,p2,e,sep="")
#   
#   raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
#   names(raw)
#   raw$status_code
#   content(raw)
#   head(raw$content)
#   neat<-rawToChar(raw$content)
#   neat1<-fromJSON(neat)
#   neat1$items[[nzchar(neat1$items)]]$prices[[1]]$price
# }
# prodprice3<-function(prodid3) {
#   
#   u<-"https://api.llbean.com/v1/products/"
#   p3<-prodid3
#   e<-"?expand=images,items,prices"
#   
#   product<-paste(u,p3,e,sep="")
#   
#   raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
#   names(raw)
#   raw$status_code
#   content(raw)
#   head(raw$content)
#   neat<-rawToChar(raw$content)
#   neat1<-fromJSON(neat)
#   neat1$items[[nzchar(neat1$items)]]$prices[[1]]$price
# }
# prodprice4<-function(prodid4) {
#   
#   u<-"https://api.llbean.com/v1/products/"
#   p4<-prodid4
#   e<-"?expand=images,items,prices"
#   
#   product<-paste(u,p4,e,sep="")
#   
#   raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
#   names(raw)
#   raw$status_code
#   content(raw)
#   head(raw$content)
#   neat<-rawToChar(raw$content)
#   neat1<-fromJSON(neat)
#   neat1$items[[nzchar(neat1$items)]]$prices[[1]]$price
# }
# 
# prodimg<-function(prodid) {
#   
#   u<-"https://api.llbean.com/v1/products/"
#   p1<-prodid
#   e<-"?expand=images,items,prices"
#   
#   product<-paste(u,p1,e,sep="")
#   
#   raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
#   names(raw)
#   raw$status_code
#   content(raw)
#   head(raw$content)
#   neat<-rawToChar(raw$content)
#   neat1<-fromJSON(neat)
#   baseimage<-neat1$images[[1]]$path
#   width<-"?wid=200"
#   paste(baseimage,width)
# }
# 
# 
# prodimg1<-function(prodid1) {
#   
#   u<-"https://api.llbean.com/v1/products/"
#   p1<-prodid1
#   e<-"?expand=images,items,prices"
#   
#   product<-paste(u,p1,e,sep="")
#   
#   raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
#   names(raw)
#   raw$status_code
#   content(raw)
#   head(raw$content)
#   neat<-rawToChar(raw$content)
#   neat1<-fromJSON(neat)
#   baseimage<-neat1$images[[1]]$path
#   width<-"?wid=200"
#   paste(baseimage,width)
# }
# prodimg2<-function(prodid2) {
#   u<-"https://api.llbean.com/v1/products/"
#   p2<-prodid2
#   e<-"?expand=images,items,prices"
#   
#   product<-paste(u,p2,e,sep="")
#   
#   raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
#   names(raw)
#   raw$status_code
#   content(raw)
#   head(raw$content)
#   neat<-rawToChar(raw$content)
#   neat1<-fromJSON(neat)
#   baseimage<-neat1$images[[1]]$path
#   width<-"?wid=200"
#   paste(baseimage,width)
# }
# prodimg3<-function(prodid3) {
#   u<-"https://api.llbean.com/v1/products/"
#   p3<-prodid3
#   e<-"?expand=images,items,prices"
#   
#   product<-paste(u,p3,e,sep="")
#   
#   raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
#   names(raw)
#   raw$status_code
#   content(raw)
#   head(raw$content)
#   neat<-rawToChar(raw$content)
#   neat1<-fromJSON(neat)
#   baseimage<-neat1$images[[1]]$path
#   width<-"?wid=200"
#   paste(baseimage,width)
# }
# prodimg4<-function(prodid4) {
#   u<-"https://api.llbean.com/v1/products/"
#   p4<-prodid4
#   e<-"?expand=images,items,prices"
#   
#   product<-paste(u,p4,e,sep="")
#   
#   raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
#   names(raw)
#   raw$status_code
#   content(raw)
#   head(raw$content)
#   neat<-rawToChar(raw$content)
#   neat1<-fromJSON(neat)
#   baseimage<-neat1$images[[1]]$path
#   width<-"?wid=200"
#   paste(baseimage,width)
# }
