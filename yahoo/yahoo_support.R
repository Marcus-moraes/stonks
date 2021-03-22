library(quantmod)
library(rvest)
library(XML)
library(lubridate)
library(tidyverse)
library(ggplot2)


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
  return(crypto)
}
