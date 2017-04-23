#install.packages(c("shiny","shinythemes","ggplot2","pROC","xts","quantmod","PerformanceAnalytics","quantmod","dygraphs","httr","jsonlite","rjson","devtools","Rforecastio","darksky"))
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


shinyUI(
  fluidPage(shinythemes::themeSelector(),
    titlePanel("Dolla'Stretch"),
 column(2, offset = -1,wellPanel( sliderInput("budget","What's your budget?",min=0,max=1000,value=100),
                      sliderInput("start_year","Pick an initial year:", min=2007,max=2017,value=2009),textInput("stock","Should you invest in this stock?",value="NVDA"),
                      selectInput("ticker1","Pick a ticker:",c('VGSIX', 'VUSTX', 'VGTSX', 'VFISX', 'VTSMX', 'VFITX', 'VEIEX', 'VIPSX'),selected = 'VFITX',multiple=FALSE),
                      selectInput("ticker2","Pick a ticker:",c('VGSIX', 'VUSTX', 'VGTSX', 'VFISX', 'VTSMX', 'VFITX', 'VEIEX', 'VIPSX'),selected = 'VUSTX',multiple=FALSE),
                      numericInput("lat","Lattitude",44.1051,min=0,max=100,step=.0001,width=NULL),
                      numericInput("lon","Longitude", -70.2009,min=0,max=100,step=.0001,width=NULL),
                      numericInput("itemid1","Type item ID to check for availability",175052,min=0,max=1000000,step=1,width=NULL),
                      sliderInput("prodid","range of ID's",min=1000,max=200000,value=c(33381,116994)),
                      selectInput("prodid1","Select the ID for Bean product:",c('33381','116998','78819','117636','63297','31179','116994','81314'),selected = '81314'),
                      selectInput("prodid2","Select the ID for Bean product:",c('33381','116998','78819','117636','63297','31179','116994','81314'),selected = '63297'),
                      selectInput("prodid3","Select the ID for Bean product:",c('33381','116998','78819','117636','63297','31179','116994','81314'),selected = '116994'),
                      selectInput("prodid4","Select the ID for Bean product:",c('33381','116998','78819','117636','63297','31179','116994','81314'),selected = '78819')
 )),
  
mainPanel(
  withMathJax(helpText("Some math here $$\\alpha+\\beta$$")),
  verbatimTextOutput("resultdesc"),
  h3(htmlOutput("result")),
  verbatimTextOutput("pricesdesc"),
  h3(htmlOutput("ar")),
  verbatimTextOutput("divdesc"),
  h3(htmlOutput("div")),
tabsetPanel(
  tabPanel("Possible Goodies",icon=icon("trophy",lib="font-awesome"),htmlOutput("inv1"),
           conditionalPanel("input.budget >= output.bean",
  htmlOutput("pic"),h3(htmlOutput("bean"))),
   conditionalPanel("input.budget >= output.bean1",
                  htmlOutput("pic1"),h3(htmlOutput("bean1"))),
  conditionalPanel("input.budget >= output.bean2",
                   htmlOutput("pic2"),h3(htmlOutput("bean2"))),
  conditionalPanel("input.budget >= output.bean3",
                   htmlOutput("pic3"),h3(htmlOutput("bean3"))),
  conditionalPanel("input.budget >= output.bean4",
                   htmlOutput("pic4"),h3(htmlOutput("bean4")))),
tabPanel("Monthly Returns",icon = icon("area-chart", lib="font-awesome"),dygraphOutput("prices")),
tabPanel("Price and Volume",imageOutput("chart1"),imageOutput("chart2")),
tabPanel("Weather",icon=icon("thermometer-half",lib="font-awesome"),htmlOutput("temp")))

)
)
)
