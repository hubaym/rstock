install.packages("sqldf")
install.packages("RPostgreSQL")

library(RPostgreSQL)
library(sqldf)

# Establish connection to PoststgreSQL using RPostgreSQL
drv <- dbDriver("PostgreSQL")
#con <- dbConnect(drv)# Simple version (localhost as default)
# Full version of connection seetting
 con <- dbConnect(drv, dbname="stock",host="localhost",port=5432,user="postgres",password="postgres")
 dbWriteTable(con, "AAPL", AAPL, row.names=FALSE)
 
 
 
 library(quantmod)
 rm(AAPLyear, AAPL)
 getSymbols("AAPL", adjust=TRUE)
 AAPLyear <- AAPL["2015"]


 AAPLyear$AAPL.WPR14 <- 0
 AAPLyear$AAPL.WPRbool <- 0
 AAPLyear$AAPL.WPRsign <- 0
 AAPLyear$AAPL.SLval <- 0
 AAPLyear$AAPL.OutSLbool <- 0
 AAPLyear$AAPL.TPval <- 0
 AAPLyear$AAPL.OutTPbool <- 0
 AAPLyear$AAPL.Broker<- 0
 AAPLyear$AAPL.Ret<- 0
 
 AAPLyear$AAPL.WPR14 <- ifelse(is.na(WPR(Cl(AAPLyear), 14)),0,WPR(Cl(AAPLyear), 14))
 AAPLyear$AAPL.WPRbool <- ifelse(AAPLyear$AAPL.WPR14 > 0.9, 1, 0)
 AAPLyear$AAPL.WPRsign <- ifelse(is.na(lag(AAPLyear$AAPL.WPRbool)),0, ifelse((AAPLyear$AAPL.WPRbool-  lag(AAPLyear$AAPL.WPRbool)>0),1,0) )      
 for (i in 2 : nrow(AAPLyear)) {
   AAPLyear$AAPL.SLval[i] <-  ifelse( is.na( AAPLyear$AAPL.SLval[i-1] )  ,0, ifelse(AAPLyear$AAPL.WPRsign[i]>0,AAPLyear$AAPL.Open[i] * 0.98,  AAPLyear$AAPL.SLval[i-1]   ))
 }
 AAPLyear$AAPL.OutSLbool <- ifelse(AAPLyear$AAPL.SLval > AAPLyear$AAPL.Close, 1, 0)
 
 for (i in 2 : nrow(AAPLyear)) {
   AAPLyear$AAPL.TPval[i] <-  ifelse( is.na( AAPLyear$AAPL.TPval[i-1] )  ,0, ifelse(AAPLyear$AAPL.WPRsign[i]>0,AAPLyear$AAPL.Open[i] * 1.06,  AAPLyear$AAPL.TPval[i-1]   ))
 }
 AAPLyear$AAPL.OutTPbool <- ifelse(AAPLyear$AAPL.TPval < AAPLyear$AAPL.Close, 1, 0)
 
 for (i in 2 : nrow(AAPLyear)-1) {
    ifelse(AAPLyear$AAPL.Broker[i]==0,   
          ifelse( AAPLyear$AAPL.WPRsign[i] >0,AAPLyear$AAPL.Broker[i+1]<- 1 , AAPLyear$AAPL.Broker[i+1]<- 0 ),
          ifelse( AAPLyear$AAPL.OutTPbool[i] >0  |  AAPLyear$AAPL.OutSLbool[i] >0,AAPLyear$AAPL.Broker[i+1]<- 0 , AAPLyear$AAPL.Broker[i+1]<- 1)
           )
 }
 AAPLyear$AAPL.Ret<- ifelse( AAPLyear$AAPL.Broker>0 ,  AAPLyear$AAPL.Close -AAPLyear$AAPL.Open,0)
 
 
 sum(AAPLyear$AAPL.Ret)
 
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
 
 


 head(AAPLyear)
 AAPLyear
 
 install.packages("ggplot2")
 library('ggplot2')
 
 xx <- 
 yy
 zz
 
 fun <- data.frame(
   x = rep(c(1:100), 100),
   y = sort(  rep(c(1:100), 100) ),
   z = c(1:10000)
 )
 ggplot(data = fun, aes(x = x, y = y, colour = z)) + geom_point(size = 1)

 
 init()
 head(AAPLyear)
 getSymbols("IBM", adjust=TRUE)
 getSymbols("AAPL", adjust=TRUE)
 
 stock<- NULL
 stocks <-  data.frame(date=index(AAPLyear), cbind( rep('AAPL',nrow(AAPLyear)),coredata(AAPLyear)))
 
###############WILLIAMS###############
 ###INIT#####
 
 WilliamsParameter <- 14
 stockColNamesExt <- c('WPRbool', 'WPRsign', 'SLval','OutSLbool','TPval','OutTPbool','Broker','Ret')

 stocknames <- c('AAPL','IBM')
 stockColNames <-  c('date','name', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
 allstocks<- nodata <- as.data.frame(setNames(replicate(8,numeric(0), simplify = F), stockColNames))
 
 for (varname in stocknames){
   getSymbols(varname, adjust=TRUE)
   x <-  data.frame(date=index(get(varname)), cbind( 
     rep(varname,nrow(get(varname))),
     coredata(get(varname)),
     ifelse(
            is.na(WPR(Cl(get(varname)), WilliamsParameter)),
            0,
            WPR(Cl(get(varname)), WilliamsParameter))
     )
     )
   colnames(x) <- c('date','name', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted', paste('WPR',WilliamsParameter, sep=""))
   allstocks <- rbind(allstocks, x)
 }
 
 for (varname in stockColNamesExt){
   allstocks[[varname]] <- 0
 }
 
 ####logic##########
 
 stoploss   <- 0.2
 takeProfit <- 0.4
 
 AAPLyear$AAPL.WPRbool <- ifelse(AAPLyear$AAPL.WPR14 > 0.9, 1, 0)
 AAPLyear$AAPL.WPRsign <- ifelse(is.na(lag(AAPLyear$AAPL.WPRbool)),0, ifelse((AAPLyear$AAPL.WPRbool-  lag(AAPLyear$AAPL.WPRbool)>0),1,0) )      
 for (i in 2 : nrow(AAPLyear)) {
   AAPLyear$AAPL.SLval[i] <-  ifelse( is.na( AAPLyear$AAPL.SLval[i-1] )  ,0, ifelse(AAPLyear$AAPL.WPRsign[i]>0,AAPLyear$AAPL.Open[i] * (1-stoploss),  AAPLyear$AAPL.SLval[i-1]   ))
 }
 AAPLyear$AAPL.OutSLbool <- ifelse(AAPLyear$AAPL.SLval > AAPLyear$AAPL.Close, 1, 0)
 
 for (i in 2 : nrow(AAPLyear)) {
   AAPLyear$AAPL.TPval[i] <-  ifelse( is.na( AAPLyear$AAPL.TPval[i-1] )  ,0, ifelse(AAPLyear$AAPL.WPRsign[i]>0,AAPLyear$AAPL.Open[i] * (1+takeProfit),  AAPLyear$AAPL.TPval[i-1]   ))
 }
 AAPLyear$AAPL.OutTPbool <- ifelse(AAPLyear$AAPL.TPval < AAPLyear$AAPL.Close, 1, 0)
 
 for (i in 2 : nrow(AAPLyear)-1) {
   ifelse(AAPLyear$AAPL.Broker[i]==0,   
          ifelse( AAPLyear$AAPL.WPRsign[i] >0,AAPLyear$AAPL.Broker[i+1]<- 1 , AAPLyear$AAPL.Broker[i+1]<- 0 ),
          ifelse( AAPLyear$AAPL.OutTPbool[i] >0  |  AAPLyear$AAPL.OutSLbool[i] >0,AAPLyear$AAPL.Broker[i+1]<- 0 , AAPLyear$AAPL.Broker[i+1]<- 1)
   )
 }
 AAPLyear$AAPL.Ret<- ifelse( AAPLyear$AAPL.Broker>0 ,  AAPLyear$AAPL.Close -AAPLyear$AAPL.Open,0)
 
 
 sum(AAPLyear$AAPL.Ret)
 
 
 
#################################################
 
head (allstocks)
