## Nowcasting one-step - recursive point-forecast with msaenet package
library(readxl)
library(tidyverse)
library(glmnet)
library(msaenet)
library(Metrics)
library(MLmetrics)
library(ggplot2)
library(stringr)

setwd("C:/Users/33640/OneDrive/Bureau/DRIM")
dlbase <- read_excel("base_diff4.xlsx")
base<-dlbase
dlbase <- data.frame(dlbase[,-c(1,2)])

#Change into numeric
for(i in (1:length(dlbase))){
  command = str_c("dlbase$",names(dlbase[i]),"<- as.numeric(dlbase$",names(dlbase[i]),")")
  eval(parse(text=command))
}

# Initialization
pred <- NULL

indepvar <- data.frame(dlbase) %>%
  select(Tx_Defaut) %>%
  #  scale(center = T, scale = T) %>%
  as.matrix()
depvar <- data.frame(dlbase) %>%
  select(-Tx_Defaut) %>% 
  #  scale(center = F, scale = F) %>%
  as.matrix()

# Nowcasting based on adaptive elastic-net
# One-step ahead forecasts for h=24
# forecast horizon h=24
#Prevision de 2018 à 2019
# for (i in 1:24) {
#   x <- data.matrix(depvar[1:(95+i),])	
#   y <- data.matrix(indepvar[1:(95+i)])
#   
#   model_cv <- aenet(x, y, family = "gaussian", init = "ridge", alphas = 0.5, tune = "cv", nfolds = 10, seed = 1001)
#   pred <- c(pred, predict(model_cv, newx = depvar[(96+i),]))	# one-step ahead forecast 
# }
# pred
# model_cv$beta
# 
# rmse(y[c(96:119)],pred)
# mse(y[c(96:119)],pred)
# mae(y[c(96:119)],pred)
# 
# MAPE(y[c(96:119)],pred[c(1:24)])   #library(MLmetrics)
# mape(y[c(96:119)],pred)   #library Metrics
# 
# MAPE(y[c(96:107)],pred[c(1:12)]) #Pour l'année 2018
# MAPE(y[c(108:119)],pred[c(13:24)]) #POur l'année 2019

#Prevision pour 2020
for (i in 1:36) {
  x <- data.matrix(depvar[1:(83+i),])	
  y <- data.matrix(indepvar[1:(83+i)])
  
  model_cv <- aenet(x, y, family = "gaussian", init = "ridge", alphas = 0.5, tune = "cv", nfolds = 10, seed = 1001)
  pred <- c(pred, predict(model_cv, newx = depvar[(84+i),]))	# one-step ahead forecast
  indepvar[84+i] <- pred[i]
}
pred

Tx <- data.matrix(base[,2])
MAPE(Tx[c(1,84)],y[c(1,84)])
MAPE(Tx[c(85:96)],pred[c(1:12)]) #Pour l'année 2017
MAPE(Tx[c(97:108)],pred[c(13:24)]) #Pour l'année 2018
MAPE(Tx[c(109:120)],pred[c(25:36)]) #Pour l'année 2019



plot(pred)


write.csv(pred,file="forecast_aenet.csv")

