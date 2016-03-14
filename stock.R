install.packages("sqldf")
install.packages("RPostgreSQL")

library(RPostgreSQL)
library(sqldf)
library(quantmod)
# Establish connection to PoststgreSQL using RPostgreSQL
drv <- dbDriver("PostgreSQL")
#con <- dbConnect(drv)# Simple version (localhost as default)
# Full version of connection seetting
 con <- dbConnect(drv, dbname="stock",host="localhost",port=5432,user="postgres",password="postgres")
 dbWriteTable(con, "AAPL", AAPL, row.names=FALSE)
 
 
 

 rm(AAPLyear, AAPL)
 getSymbols("AAPL", adjust=TRUE)
 AAPLyear <- AAPL["2015"]



 
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

 

 #################COMMON PARAMETERS############
 setParameters <- function(){
   parameter.stockColNames <<-  c('date','name', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
   parameter.stocknames <<- c('AAPL','IBM')
   parameter.spL   <<- 0.02
   parameter.tpL   <<- 0.04
 }
 
 
###############WILLIAMS###############
 williamsTest <- function(){
 parameter.stocknameExt <- c('WPRbool', 'WPRsign', 'SLval','OutSLbool','TPval','OutTPbool','Broker','Ret')
 williams.par <- 14
 williams.var <- paste('WPR',williams.par, sep="")
 williams.limit <- 0.9
 
 
 ###INIT#####
 williams.resultnames <- c('stockname','WPAr', 'slL', 'tpL', 'result')
  results <- as.data.frame(setNames(replicate(8,numeric(0), simplify = F), williams.resultnames))
  allstocks <<- as.data.frame(setNames(replicate(8,numeric(0), simplify = F), parameter.stockColNames))
 
 for (varname in parameter.stocknames){
   getSymbols(varname, adjust=TRUE)
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
 
 for (varname in parameter.stocknameExt){
   allstocks[[varname]] <- 0
 }
 
 ####logic##########
 as.numeric(as.character(allstocks[[williams.var]][30])) -
   as.numeric(as.character(allstocks[[williams.var]][30]))
 
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
          as.numeric(as.character(allstocks$Open[i])) * (1-parameter.spL),
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
 
 result <- c('ALL', williams.par,parameter.spL,parameter.tpL, sum(allstocks$Ret))

 results <<- rbind(results, result)
 colnames(results) <- williams.resultnames
 
 }
 
 
#################################################
 
setParameters()
williamsTest()

rm(results)
 
 
 
 
 
 
