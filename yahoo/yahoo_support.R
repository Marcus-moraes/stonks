library(quantmod)
library(rvest)
library(XML)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(caret)
library(forecast)


crypto_fun <- function(){
  url1 <- "https://finance.yahoo.com/cryptocurrencies?guce_referrer=aHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS8&guce_referrer_sig=AQAAAJyd-jwq46gSgEkpgzfO3ZCf7oW-LeCpwXw24Z2UwzQv_6FFNYDm3guwDuwXW4jVrGU3eS_NzqVQ1V6UiwRMU_cgbXiLxiXX5AA6RNKPxmhsqKWqnTWI6rkLOd-UHPnbNVMqV3vepzKSvk_hgtFo-R3Bf3Edf8tj58u7Cz4B0NYE&offset=0&count=100"
  url2 <- "https://finance.yahoo.com/cryptocurrencies?count=100&guce_referrer=aHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS8&guce_referrer_sig=AQAAAJyd-jwq46gSgEkpgzfO3ZCf7oW-LeCpwXw24Z2UwzQv_6FFNYDm3guwDuwXW4jVrGU3eS_NzqVQ1V6UiwRMU_cgbXiLxiXX5AA6RNKPxmhsqKWqnTWI6rkLOd-UHPnbNVMqV3vepzKSvk_hgtFo-R3Bf3Edf8tj58u7Cz4B0NYE&offset=100"
  url3 <- "https://finance.yahoo.com/cryptocurrencies?count=100&guce_referrer=aHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS8&guce_referrer_sig=AQAAAJyd-jwq46gSgEkpgzfO3ZCf7oW-LeCpwXw24Z2UwzQv_6FFNYDm3guwDuwXW4jVrGU3eS_NzqVQ1V6UiwRMU_cgbXiLxiXX5AA6RNKPxmhsqKWqnTWI6rkLOd-UHPnbNVMqV3vepzKSvk_hgtFo-R3Bf3Edf8tj58u7Cz4B0NYE&offset=200"
  url4 <- "https://finance.yahoo.com/cryptocurrencies?count=100&guce_referrer=aHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS8&guce_referrer_sig=AQAAAJyd-jwq46gSgEkpgzfO3ZCf7oW-LeCpwXw24Z2UwzQv_6FFNYDm3guwDuwXW4jVrGU3eS_NzqVQ1V6UiwRMU_cgbXiLxiXX5AA6RNKPxmhsqKWqnTWI6rkLOd-UHPnbNVMqV3vepzKSvk_hgtFo-R3Bf3Edf8tj58u7Cz4B0NYE&offset=300"
  
  url <- c(url1,url2,url3,url4)
  
  crypto = NULL
  
  for (i in seq_along(url)){
    texto <- read_html(url[i])
    teste <- texto%>%html_nodes("table")
    teste2 <- htmlParse(teste, asText = T)
    df <- xpathSApply(teste2,"//a[ contains(@class , 'C($linkColor)') ]",xmlValue)
    df <- df[df!=""]
    crypto <- append(crypto,df)
    
  
  }
  crypto <- crypto[order(crypto)]
  # previsao(crypto)
  return(crypto)
}





previsao <- function(crypto){
  
  from <- Sys.Date()-200
  to <- Sys.Date()
  data <- getSymbols(crypto, src = "yahoo", auto.assign = F,
                     from = from,
                     to = to,
                     return.class = "zoo")
  # createTimeSlices(y = data  ,initialWindow = nrow(data)*.8, horizon = 2)
  # names(data) <-  c("open","high","low","close","volume","adjusted")
  # data <- data[,-6]
  teste <- tail(data, n = 10)
  treino <- head(data, n=.9*nrow(data))
  # return(data)
  ##############################
  
  # close <- treino[,4]
  # treino <- treino[,-4]
  # treino <- as.data.frame(treino)
  # teste <- as.data.frame(teste)
  teste2 <- teste[,-1]
  
  
  quantummodel <- specifyModel(Next(OpCl(data))~Lag(OpHi(data),0:3)+Lag(Vo(data),0:3))
  quantumfit <- buildModel(quantummodel, method = "lm", training.per = c(as.character(from) ,as.character(to-1)))
  
  
  
   return(fittedModel(quantumfit))
  # fit <- tslm(formula = close~open+high+low+volume+trend, data = treino, lambda = NULL)
  
  fit <- fittedModel(quantumfit)
  supdata <- quantummodel@model.data
  newdata <- tail(supdata, n = 1)
  cresc_perc <- predict(fit, newdata= newdata[,-1])
  
  # return(fit)
  
}













