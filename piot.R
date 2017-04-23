#remove the pesky hashtag below, run the script,
#and now you have a function which can find the F-score for any stock, hopefully.
#For example, for Amazons F score, go to the console and enter piot("AMZN")

#install.packages('quantmod')
require(quantmod)

piot<-function(ticker) {
F=0
historicprices<-getSymbols(ticker,src='yahoo',auto.assign = FALSE)
finance<-getFinancials(ticker, src = 'google',auto.assign = FALSE)
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
#price series
chartSeries(historicprices, type = 'line',show.grid = TRUE) 
paste(print(F),"is the Piotroski F score for",print(ticker), sep=" ")

}

