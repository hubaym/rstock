install.packages("sqldf")
install.packages("RPostgreSQL")

library(RPostgreSQL)
library(sqldf)
library(quantmod)
# Establish connection to PoststgreSQL using RPostgreSQL
drv <- dbDriver("PostgreSQL")
#con <- dbConnect(drv)# Simple version (localhost as default)
# Full version of connection seetting
 con <- dbConnect(drv, dbname="stock",host="localhost",port=5432,user="postgres",password="")


 
 plot(AAPLyear)
 for (i in 2 : nrow(AAPLyear)-1) {
   if(AAPLyear$AAPL.Broker[i]>0)  {
     
     abline(v= .index(AAPLyear)[i], col="blue")
   } 
   if(AAPLyear$AAPL.WPRsign[i]>0)  {
     
     abline(v= .index(AAPLyear)[i], col="green")
   } 
   if(AAPLyear$AAPL.OutSLbool[i]>0  | AAPLyear$AAPL.OutTPbool[i]>0)  {
     
     abline(v= .index(AAPLyear)[i], col="red")
   }  
   
 }

 install.packages("ggplot2")
 library('ggplot2')
 

 fun <- data.frame(
   x = rep(c(1:100), 100),
   y = sort(  rep(c(1:100), 100) ),
   z = c(1:10000)
 )
 ggplot(data = fun, aes(x = x, y = y, colour = z)) + geom_point(size = 1)

##############INITSTOCKs function##################
 initStocks<- function(stockNameList){
   
   for (stock in stockNameList){
     
     if ( !dbExistsTable(con,stock)){
       print(stock)
       getSymbols(stock, adjust=TRUE)
       dbWriteTable(con, stock, data.frame(date=index(get(stock)) ,as.data.frame(get(stock), 
                c("Open","High","Low","Close","Volume","Adjusted")), row.names=TRUE))
       
     }
     x <- dbReadTable(con, stock)
     assign(stock, xts(x[,-1], order.by=x[,1])["2015"], envir = .GlobalEnv)
     rm(x)
   }
   
 }
 
###############WILLIAMS###############
 williamsTest <- function(){
 parameter.stocknameExt <- c('WPRbool', 'WPRsign', 'SLval','OutSLbool','TPval','OutTPbool','Broker','Ret')
 williams.par <- 14
 williams.var <- paste('WPR',williams.par, sep="")
 williams.limit <- 0.9
 
 
 ###INIT#####
 williams.resultnames <- c('stockname','WPAr', 'slL', 'tpL', 'result')
  results <<- as.data.frame(setNames(replicate(5,numeric(0), simplify = F), williams.resultnames))
  
  
  
  for (spLLocal in parameter.spL){ 
    print("startloop")
    print(spLLocal)
    
    allstocks <<- as.data.frame(setNames(replicate(8,numeric(0), simplify = F), parameter.stockColNames))
    
 for (varname in parameter.stocknames){
   x <-  data.frame(date=index(get(varname)), cbind( 
     rep(varname,nrow(get(varname))),
     coredata(get(varname)),
     ifelse(
            is.na(WPR(Cl(get(varname)), williams.par)),
            0,
            WPR(Cl(get(varname)), williams.par))
     )
     )
   colnames(x) <- c(parameter.stockColNames,williams.var)
   allstocks <- rbind(allstocks, x)
 }
    print("ready with stocknames")
 
 for (varname in parameter.stocknameExt){
   allstocks[[varname]] <- 0
 }
 
 ####logic##########
 
 allstocks$WPRbool <- ifelse(
   as.numeric(as.character(allstocks[[williams.var]])) > williams.limit,      #above limit condition
   1,                                               
   0)
 
 for (i in 2 : nrow(allstocks)) {
     allstocks$WPRsign[i] <- 
       ifelse(
         (allstocks$WPRbool[i] - allstocks$WPRbool[i-1]>0 &           #WPRbool 0 -> 1 change condition
            allstocks$name[i] == allstocks$name[i-1]),  
         as.numeric(as.character(allstocks[[williams.var]][i])) -     #measure sign strenghs
           as.numeric(as.character(allstocks[[williams.var]][i-1])),
         0)
 }

 
  for (i in 2 : nrow(allstocks)) {
   allstocks$SLval[i] <-  
     ifelse( allstocks$name[i]== allstocks$name[i-1],
        ifelse(
          allstocks$WPRsign[i-1]>0,                    # calculate stoploss value at step in
          as.numeric(as.character(allstocks$Open[i])) * (1-spLLocal),
          allstocks$SLval[i-1] ),
     0)
  }

 for (i in 2 : nrow(allstocks)) {
   allstocks$OutSLbool[i] <- 
     ifelse(                                         #Step out condition based on SL
       as.numeric(as.character(allstocks$SLval[i])) > 
        as.numeric(as.character(allstocks$Close[i])),
        1,
        0)
 }
 allstocks$TPval <- 0
 allstocks$TPval[1] <- 10000
 for (i in 2 : nrow(allstocks)) {                #calculate Takeprofit at step in
   allstocks$TPval[i] <-  
     ifelse( allstocks$name[i]== allstocks$name[i-1],
        ifelse(
          allstocks$WPRsign[i-1]>0,
          as.numeric(as.character(allstocks$Open[i])) * (1+parameter.tpL),
          allstocks$TPval[i-1]   ),
     0)
 }
 for (i in 2 : nrow(allstocks)) {
   allstocks$OutTPbool[i] <- 
     ifelse(                                         #Step out condition based on TP
       as.numeric(as.character(allstocks$TPval[i-1])) <  
         as.numeric(as.character(allstocks$Close[i-1])),
       1,
       0)
 }
 print("before broker")
 allstocks$Broker[i]<- 0
 for (i in 2 : nrow(allstocks)) {
   ifelse( allstocks$name[i]== allstocks$name[i-1],
           
      ifelse(allstocks$Broker[i-1]==0,   
            ifelse( allstocks$WPRsign[i] >0,
                    allstocks$Broker[i]<- 1 , 
                    allstocks$Broker[i]<- 0 ),
            ifelse( allstocks$OutTPbool[i] >0  |  allstocks$OutSLbool[i] >0,
                    allstocks$Broker[i]<- 0 , 
                    allstocks$Broker[i]<- 1)),
      
   allstocks$Broker[i]<- 0)
 }
 
 allstocks$Ret<- ifelse( allstocks$Broker>0 ,
                              as.numeric(as.character(allstocks$Close)) - 
                                as.numeric(as.character(allstocks$Open)),
                              0)
 
 print (sum(allstocks$Ret))
 result <- data.frame('ALL', williams.par,spLLocal,parameter.tpL, sum(allstocks$Ret))
 
 results <<- rbind(results, result)
 rm(result)
 rm(allstocks)
 
  }
 colnames(results) <- williams.resultnames
 
 }
 
 
#################################################
 
 #################COMMON PARAMETERS############

   parameter.stockColNames <<-  c('date','name', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
   parameter.stocknames <<- c('AAPL','IBM')
   parameter.spL   <<- seq(0.016, 0.018, 0.001)
   parameter.tpL   <<- 0.04
 
   
   
   
   initStocks(parameter.stocknames)
   rm(allstocks)
williamsTest()

rm(results)
rm(IBM)
rm(AAPL)
rm(allstocks)
rm(williams.resultnames)
warnings()
 
data.frame('ALL','3','eer',45)
 
 
getSymbols("IBM", adjust=TRUE)
get("IBM")<-setNames(get("IBM"), c("Open","High","Low","Close","Volume","Adjusted"))
dbWriteTable(con, "IBM", data.frame(date=index(get("IBM")) ,get("IBM")), row.names=FALSE)

names(IBM) <-c("Open","High","Low","Close","Volume","Adjusted")
IBM[1,]

head(as.data.frame(IBM, c("Open","High","Low","Close","Volume","Adjusted")))
setNames(get(stock), c("Open","High","Low","Close","Volume","Adjusted"))
dbWriteTable(con, stock, data.frame(date=index(get(stock)) ,get(stock)), row.names=FALSE)


dbWriteTable(con, "IBM", data.frame(date=index(get("IBM")) ,as.data.frame(get("IBM"), 
                                                                          c("Open","High","Low","Close","Volume","Adjusted")), row.names=TRUE))
