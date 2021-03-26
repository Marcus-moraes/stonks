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
    closeAllConnections()
  
  }
  crypto <- crypto[order(crypto)]
  # previsao(crypto)
  closeAllConnections()
  return(crypto)
  
}

fatores <- function(df){
  # df <- lista2
  fator.reacao <- NULL
  fator.trend <- NULL
  fator.final.mean <- NULL
  for(i in seq_along(df)){
    
    media.3d <- mean(tail(df[[i]][,2],n=3),na.rm = T)
    media.7d <- mean(tail(df[[i]][,2],n=7),na.rm=T)
    valor.atual <- tail(df[[i]][,2],n=1)
    fator.reacao[i] <- media.3d/valor.atual
    fator.trend[i] <- media.3d/media.7d
    # print(fator.reacao)
  }
  # print(fator.trend)
  for(i in seq_along(df)){
    # print(i)
    nome <- strsplit(names(lista2[[i]])[1],"\\.")[[1]][1]
    if(fator.trend[i]>1 & fator.reacao[i]>1 ){
      fator.final.mean[i] = 2*(fator.trend*fator.reacao-1)
      
    }
    else if(fator.trend[i]>1 & fator.reacao[i]<1 ){
      fator.final.mean[i] = 1.5*(fator.trend*fator.reacao-1)
      
    }
    else if(fator.trend[i]< 1 & fator.reacao[i]>1){
      fator.final.mean[i] = -2*(fator.trend*fator.reacao-1)
      
    }
    else if(fator.trend[i]<= 1 & fator.reacao[i]<= 1)
      fator.final.mean[i] = 1*(fator.trend*fator.reacao-1)
    
    
  }
  
  return(fator.final.mean)
}

removena <- function(dfbase){
  df <- impute.knn(as.matrix(dfbase))$data
  vetorzero <- df==0 
  df[vetorzero]=.000001
  df <- as.zoo(df,seq(from= from, to=to, by=1))
  return(df)
  
  
}

retiramenor <- function(listao){ 
  listao2 <- NULL
  defective <- NULL
  j=0
  n=0
  for(i in seq_along(listao)){
    r <- nrow(listao[[i]])
    relacao <- r/sum(is.na(listao[[i]]),listao[[i]]<.00001,na.rm=T)
    
    if(r > 79){
      if(relacao > 3){
        
        j=j+1
        listao2[[j]] <- removena(listao[[i]])
        
      }
      
    } 
    else{
      n=n+1
      defective[[n]] <- listao[[i]] 
    }
    
    
  }
  return(listao2)
}


  
data.env <- new.env()
from <- Sys.Date()-200-2
to <- Sys.Date()-2
final <- NULL
lista2 <- retiramenor(lista)
numes <- NULL
fator.ponderado <- NULL
 

### acessa yahoo e retira todos os dados para novo enviroment-----    
for(i in seq_along(muedas)){
  crypto <- muedas[i]
  tryCatch(suppressWarnings(getSymbols(crypto, src = "yahoo", auto.assign = T,
                                       from = from,
                                       to = to,
                                       return.class = "zoo",
                                       warnings = F,
                                       env = data.env)),
           error=function(e){NULL})
  closeAllConnections()
  
}       

### constrói todos os modelos e realiza previsao-----
for(i in seq_along(lista2)){
  
  data <- lista2[[i]]
  nome <- strsplit(names(data)[1],"\\.")[[1]][1]
  quantummodel <- specifyModel(Next(OpCl(data))~Lag(OpHi(data),0:3)+Lag(Vo(data),0:3))
  quantummodel <- getModelData(quantummodel)
  set.seed(21902393)
  quantumfit <- buildModel(quantummodel, method = "lm", training.per = c(as.character(from) ,as.character(to-1)))
  fit <- fittedModel(quantumfit)
  supdata <- quantummodel@model.data
  newdata <- tail(supdata, n = 1)
  cresc_perc <- 100*predict(fit, newdata= newdata[,-1])
  names(cresc_perc) <- nome
  numes[i] <- nome
  final[i] <- cresc_perc
  # print(nome)
}


fator.media <- fatores(lista2)
names(fator.media) <- numes
fator.fit <- final

for(i in seq_along(fator.media)){
  if(fator.fit[i] > 3 | fator.fit[i] < (-3)){
    fator.ponderado[i] <- fator.media[i]
  }
  else{
    fator.ponderado[i] <- mean(2*fator.media[i] +fator.fit[i])
  }
  
}             

fator.ponderado <- fator.ponderado
names(fator.ponderado) <- numes




