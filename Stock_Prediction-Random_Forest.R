install.packages("quantmod")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ROCR")
install.packages("caret")
install.packages("randomForest")
install.packages("plotly")
library(quantmod)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(randomForest)
library(plotly)
#start and end dates
start <- as.Date("2015-01-01")
end <- as.Date("2019-03-25")
#Prediction window set to 20. 
window <- 20
#Using 4 companies historical data for preditcion
stocks <- c("AAPL","NFLX", "MSFT", "AMZN")
getSymbols(stocks, src='yahoo', from=start, to=end)
get_indicators <- function(stock, window){
  #creating response variable. Predicting next days price, by using lag function in price_change
  price_change <- Ad(lag(stock,-window)) - Ad(stock)
  response <- ifelse(price_change > 0, "UP", "DOWN")
  #Calculating RSI
  rsi <- RSI(Ad(stock), n=14)
  #High, Low, and adjusted close xts object
  hlac <- as.xts(data.frame(x=Hi(stock), y=Lo(stock), z=Ad(stock)))
  
  #Stochastic Oscillator
  sto <- stoch(hlac, nFastK = 14) *100
  #Williams %R
  wpr <-WPR(hlac, n=14) * (-100)
  
  #MACD
  macd <- MACD(Ad(stock), nFast=12, nSlow=26, nSig=9)  
  #Price Rate of Change
  roc <- ROC(Ad(stock), n=14) *100
  #On Balance Volume
  obv <- OBV(Ad(stock), Vo(stock))
  
  #create data set with all indicators and labeled columns 
  indicators <- data.frame(rsi, sto, wpr, macd, roc, obv, response)
  colnames(indicators) <- c("RSI", "StoFASTK","StoFASTD","StoSLOWD", 
                            "WilliamPR", "MACD","MACDSignal", "PriceRateOfChange", 
                            "OnBalanceVolume",
                            "Response")
  #removing na values from calculations and keeping sizes of columns same
  indicators <- indicators[-1:-35,]
  
  #removing na values due to lag
  indicators <- head(indicators,-window)
  return(indicators)
}
get_histogram <- function(company) {
  y = c() 
  for (i in 1:20) {
    comp <- get_indicators(company, i)
    per <- round(table(comp$Response)[[2]] / (table(comp$Response)[[1]] + table(comp$Response)[[2]]),3)
    y <- c(y,as.character(per))
  }
  return(y)
}
x <- c()
for (i in 1:20){
  x <- c(x, as.character(i))
}
xform <- list(categoryorder = "array",
              categoryarray = x)
plot_histogram <- function(){
  y = get_histogram(AAPL)
  p1 <- plot_ly(y=y, x=x, histfunc='sum', type = "histogram",legendgroup = "l", name = "AAPL") %>%
    layout(yaxis=list(type='linear'),
           hovermode = 'x',
           xaxis = xform,
           title = "Percentage of Up Days")
  y = get_histogram(NFLX)
  p2 <- plot_ly(y=y, x=x, histfunc='sum', type = "histogram", name = "NFLX") %>%
    layout(yaxis=list(type='linear'),
           hovermode = 'x',
           xaxis = xform,
           title = "Percentage of Up Days")
  y = get_histogram(MSFT)
  p3 <- plot_ly(y=y, x=x, histfunc='sum', type = "histogram", name = "MSFT") %>%
    layout(yaxis=list(type='linear'),
           hovermode = 'x',
           xaxis = xform,
           title = "Percentage of Up Days")
  y = get_histogram(AMZN)
  p4 <- plot_ly(y=y, x=x, histfunc='sum', type = "histogram", name = "AMZN") %>%
    layout(yaxis=list(type='linear'),
           hovermode = 'x',
           xaxis = xform,
           title = "Percentage of Up Days")
  subplot(p1,p2,p3, p4, nrows = 2)
}


predict_accuracy <- function(model){
  #create predictions off model
  pred <- predict(model, test, type="class")
  acc <- confusionMatrix(pred, test$Response)$overall[[1]]
  return(acc)
}
roc_graph <- function(model){
  pred.roc <- predict(model, test, type="prob")[,2]
  f.pred <- prediction(pred.roc, test$Response)
  f.perf <- performance(f.pred, "tpr", "fpr")
  
  auc <- performance(f.pred, measure = "auc")
  
  plot(f.perf, colorize=T, lwd=3, 
       main="ROC Cruve", sub= 
         paste("\nThe area under curve (AUC) for this model is ", round(auc@y.values[[1]], 3)))
  abline(0,1)
}

plot_accuracy <- function(){
  x <- c(1:20)
  plot_ly(accuracys, x= ~x, y= ~accuracys$AAPL, name = 'AAPL', type = 'scatter', mode = 'lines', width = 950) %>%
    add_trace(y = ~accuracys$NFLX, name = 'NFLX', mode = 'lines') %>%
    add_trace(y = ~accuracys$MSFT, name = 'MSFT', mode = 'lines') %>%
    add_trace(y = ~accuracys$AMZN, name = 'AMZN', mode = 'lines') %>%
    layout(title = 'Accuracy',
           xaxis = list(title = 'Days Ahead',
                        zeroline = TRUE,
                        range = c(0, 20)),
           yaxis = list(title = 'Accuracy',
                        range = c(0.48,.85)),
           hovermode='x')
}

set.seed(100)
#temp row in df

accuracys <- data.frame(rep(0,window))
for(stock in stocks){
  stock <- get(stock)
  accuracy.day <- c()
  for(day in 1:window){
    stock_indicators <- get_indicators(stock,day)
  
    #Split the data in a 80-20 (train-test) ratio.
    index <- sample(1:nrow(stock_indicators), size=0.2*nrow(stock_indicators))
    test <- stock_indicators[index, ]
    train <- stock_indicators[-index, ]
    
    #creating model
    model <- randomForest(train$Response ~ ., train, importance=TRUE, ntree=200, mtry=4)
    
    #vector containing accuracies from 1-window
    accuracy.day <- c(accuracy.day,predict_accuracy(model))
  }
  accuracys <- cbind(accuracys,data.frame(ticker=accuracy.day))
}
#remove temp row in df
accuracys <- accuracys[,-1]
colnames(accuracys) <- stocks

plot_histogram()
plot_accuracy()
roc_graph(model)
predict_accuracy(model)
