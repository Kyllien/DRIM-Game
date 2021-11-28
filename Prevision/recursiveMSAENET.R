## Nowcasting one-step - recursive point-forecast with msaenet package
library(readxl)
library(tidyverse)
library(glmnet)
library(msaenet)
library(Metrics)
library(MLmetrics)
library(ggplot2)

setwd("C:/Users/33640/OneDrive/Bureau/DRIM")
dlbase <- read.csv("base_diff.csv")
dlbase <- data.frame(dlbase[,-c(1,2)])
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
for (i in 1:24) {
  x <- data.matrix(depvar[1:(95+i),])	
  y <- data.matrix(indepvar[1:(95+i)])
  
  model_cv <- aenet(x, y, family = "gaussian", init = "ridge", alphas = 0.5, tune = "cv", nfolds = 10, seed = 1001)
  pred <- c(pred, predict(model_cv, newx = depvar[(96+i),]))	# one-step ahead forecast 
}
pred
model_cv$beta

rmse(y[c(96:119)],pred)
mse(y[c(96:119)],pred)
mae(y[c(96:119)],pred)

MAPE(y[c(96:119)],pred)   #library(MLmetrics)
mape(y[c(96:119)],pred)   #library Metrics

MAPE(y[c(96:107)],pred[c(1:12)]) #Pour l'année 2018
MAPE(y[c(108:119)],pred[c(13:24)]) #POur l'année 2019

#Prevision pour 2020
for (i in 1:12) {
  x <- data.matrix(depvar[1:(118+i),])	
  y <- data.matrix(indepvar[1:(118+i)])
  
  model_cv <- aenet(x, y, family = "gaussian", init = "ridge", alphas = 0.5, tune = "cv", nfolds = 10, seed = 1001)
  pred <- c(pred, predict(model_cv, newx = depvar[(119+i),]))	# one-step ahead forecast
  indepvar[119+i] <- pred[24+i]
}
pred

plot(pred)

write.csv(pred,file="forecast_aenet.csv")


