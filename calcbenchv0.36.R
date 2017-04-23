#run the following command first to install required packages
#install.packages(c("rjson","jsonlite","RJSONIO","httr","shiny","rPython","quantmod","shinythemes"))
require(rjson)
require(jsonlite)
require(RJSONIO)
require(httr)
require(shiny)
require(shinythemes)
require(quantmod)
require(rPython)
require(RCurl)
require(plyr)

ui <- fluidPage(shinythemes::themeSelector(),
          titlePanel("Calcbench v0.35"),
          column(2, offset = -5,wellPanel(
            textInput('siccode','Enter an Industry SIC code:',value="2834",placeholder = "1200"),
            textInput('collection','Pick an Index (DJIA or SP500):',value="DJIA",placeholder = "SP500"),
            textInput('commodity','check Piotroski F-Score and Price Series for stock:',value="MSFT",placeholder = "AAPL")
          )),
          column(2,offset=-3,wellPanel(
            textInput('venture1','Pick companies for which you want FaceStatements:',value = "AIVN",placeholder = "AIVN"),
            textInput('venture2','',value="EWST",placeholder="OPMZ")
          )),
          column(3,offset=-1,wellPanel(
            textInput('address1','Type any generic address:',value="Nixor College, Karachi, Pakistan",placeholder = "Times Square, New York, NY"),
            textInput('address2','Type any generic address:',value="Bates College, Lewiston, Maine",placeholder = "Times Square, New York, NY")  
          )),
          mainPanel(tabsetPanel(
            tabPanel("Industry Tickers",icon = icon("bank", lib="font-awesome"),verbatimTextOutput("tikerzz")),
            tabPanel("Piot F-score and Price Series",icon = icon("area-chart", lib="font-awesome"),plotOutput("pricechart"),verbatimTextOutput("piotroski")),
            tabPanel("I Maps",icon = icon("map", lib="font-awesome"),tableOutput("map")),
            tabPanel("Income Statement",icon = icon("info", lib="font-awesome"),verbatimTextOutput("is")),
            tabPanel("Balance Sheet",icon = icon("calculator", lib="font-awesome"),verbatimTextOutput("bs"))
          )
))   

server <- function(input, output) {
  
industry<-function(siccode) {
    cb_email <- "irajwani@bates.edu"
    cb_password <- "Fangtasful01"
    p<-POST(url="https://www.calcbench.com/account/LogOnAjax",'email'=cb_email,'strng'=cb_password,'rememberMe'=TRUE,verify=FALSE)
    permnanentpiece<-"https://www.calcbench.com/api/companies?siccodes="
    alterablepiece<-siccode
    product<-paste(permnanentpiece,alterablepiece,sep="")
    pharmas<-GET(url=product)
    neatpharmas<-rawToChar(pharmas$content)
    neatpharmas1<-fromJSON(neatpharmas)
    len<-length(neatpharmas1)
    for(j in 1:len) {
      c<-neatpharmas1[[j]]$ticker
      d<-neatpharmas1[[j]]$most_recent_filing
      e<-neatpharmas1[[j]]$entity_name
      show(paste(c,e,d,sep="       "))
    }
  }
  
indices<-function(commodity,collection){
    # python.exec("import pip")
    # python.exec("package_name='git+http://github.com/calcbench/python_api_client.git' ")
    # python.exec("pip.main(['install', package_name])")
    python.exec("import calcbench")
    python.exec("import json")
    python.exec("calcbench.set_credentials('irajwani@bates.edu','Fangtasful01')")
    c<-collection
    python.assign("c",c)
    python.exec("tickers = calcbench.tickers(index=c)")
    basket<-python.get("tickers")
    for(k in 1:length(basket)+1){
      if(basket[k-1] == commodity) {
        gs<-getSymbols(basket[k-1],src = 'yahoo',auto.assign = FALSE)
        chartSeries(gs,name = commodity) }
                               }
    for(k in 1:length(basket)+1) {
      if(basket[k-1]==commodity) {
        F=0
        finance<-getFinancials(basket[k-1], src = 'google',auto.assign = FALSE)
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
        paste(print(F),"is the Piotroski F score for",print(commodity), sep=" ")
      }
    }
    
    }

incomestatement<-function(venture1,venture2){
  python.exec("import calcbench as cb")
  python.exec("import json")
  python.exec("import datetime")
  python.exec("calcbench.set_credentials('irajwani@bates.edu','Fangtasful01')")
  python.exec("income_statement_metrics = [
    'Revenue',
              'CostOfRevenue',
              'GrossProfit',
              'SGAExpense',
              'OperatingExpenses',
              'OperatingIncome',
              'EBIT',
              'InterestExpense',
              'IncomeTaxes',
              'NetIncome'
              ]")
  python.exec("balance_sheet_metrics = [
    'Cash',
              'AccountsReceivable',
              'Inventory',
              'CurrentAssets',
              'PPE',
              'Goodwill',
              'LongTermInvestments',
              'Assets',
              'AccountsPayable',
              'ShortTermDebt',
              'DeferredRevenue',
              'CurrentLiabilities',
              'LongTermDebt',
              'Liabilities',
              'RetainedEarnings',
              'TreasuryStockValue',
              'StockHoldersEquity',
              
              ]")
  one<-venture1
  two<-venture2
  python.assign("one",one)
  python.assign("two",two)
  python.exec("v=cb.normalized_raw(company_identifiers=[one,two], metrics=income_statement_metrics, update_date=datetime.date(2017, 1, 23), all_history=True)")
  grocerylist<-python.get("v")
  print(grocerylist)
  
}
balancesheet<-function(venture1,venture2){
  python.exec("import calcbench as cb")
  python.exec("import json")
  python.exec("import datetime")
  python.exec("calcbench.set_credentials('irajwani@bates.edu','Fangtasful01')")
  python.exec("income_statement_metrics = [
              'Revenue',
              'CostOfRevenue',
              'GrossProfit',
              'SGAExpense',
              'OperatingExpenses',
              'OperatingIncome',
              'EBIT',
              'InterestExpense',
              'IncomeTaxes',
              'NetIncome'
              ]")
  python.exec("balance_sheet_metrics = [
              'Cash',
              'AccountsReceivable',
              'Inventory',
              'CurrentAssets',
              'PPE',
              'Goodwill',
              'LongTermInvestments',
              'Assets',
              'AccountsPayable',
              'ShortTermDebt',
              'DeferredRevenue',
              'CurrentLiabilities',
              'LongTermDebt',
              'Liabilities',
              'RetainedEarnings',
              'TreasuryStockValue',
              'StockHoldersEquity',
              
              ]")
  one<-venture1
  two<-venture2
  python.assign("one",one)
  python.assign("two",two)
  python.exec("v=cb.normalized_raw(company_identifiers=[one,two], metrics=balance_sheet_metrics, update_date=datetime.date(2017, 1, 23), all_history=True)")
  grocerylist<-python.get("v")
  print(grocerylist)
  
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
  x <- fromJSON(doc,simplify = FALSE)
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

geocodevec<- function(address1,address2,verbose=FALSE){
  if(verbose) cat(address1,"\n")
  if(verbose) cat(address2,"\n")
  a <- c(address1,address2)
  locations <- ldply(a, function(x) geocode(x))
  names(locations) <- c("lat","lon","location_type", "forAddress")
  locations
}
#Management Discussion and Analysis Document Type
# mddtype<-function(mddcode,limit){
#   python.exec("import calcbench as cb")
#   python.exec("import json")
#   python.exec("from IPython.core.display import display, HTML")
#   x<-limit
#   python.assign("x",x)
#   python.exec("tickers = cb.tickers(index='DJIA')[:x]")
#   ffff<-mddcode
#   python.assign("ffff",ffff)
#   python.exec("Management_Discussion_and_Analysis_Document_Type=ffff") 
#   python.exec("for ticker in tickers: 
#               found_document=list(cb.document_search(company_identifiers=[ticker],document_type=Management_Discussion_and_Analysis_Document_Type,year=2014,period=0))")
#   python.exec("g=ticker")
#   v<-python.get( "g" )
#   print(v)
#   
#   }
  
  output$tikerzz<-renderPrint({industry(input$siccode)})
  output$pricechart<-renderPlot({indices(input$commodity,input$collection)})
  output$piotroski<-renderPrint({indices(input$commodity,input$collection)})
  output$is <- renderPrint({incomestatement(input$venture1,input$venture2)})
  output$bs<-renderPrint({balancesheet(input$venture1,input$venture2)})
  output$map<-renderTable({geocodevec(input$address1,input$address2)})
}

shinyApp(ui,server)



# metrics <- c('revenue','netincome','assets','stockholdersequity')
# payload <- list("start_year" = 2010, 
#             'start_period' = 1,
#              'end_year' = 2014,
#             'end_period' = 4,
#   'company_identifiers' = c('ibm','goog'),
#   'metrics' = metrics)
# 
# normalized<-POST(url="https://www.calcbench.com/api/NormalizedValues",body = payload,encode = "json",verify=TRUE)
# asreportedata<-GET(url="https://www.calcbench.com/api/asreported/?companyIdentifier=ibm&statementType=income&periodType=annual")
# 
# GET(url="https://www.calcbench.com/api/footnoteSearch/companies?entireUniverse=true/period?year=2008&period=0&end_year=2012&end_period=0/page?allFootnotes=true&fullTextQuery=test")


