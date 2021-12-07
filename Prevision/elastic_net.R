## Nowcasting one-step - recursive point-forecast with elastic net
library(readxl)
library(tidyverse)
library(lgarch)		# Gets modelling
library(gets)
library(glmnet)
library(Metrics)
library(MLmetrics)

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

# Nowcasting based on elastic-net
# One-step ahead forecasts for h=12
# forecast horizon h=12
for (i in 1:36) {
  x <- data.matrix(depvar[1:(83+i),])	
  y <- data.matrix(indepvar[1:(83+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  en_cv <- cv.glmnet(x, y, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cv <- en_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0.5, lambda = lambda_cv, standardize = T)
  pred <- c(pred, predict(model_cv, newx = depvar[(84+i),]))	# one-step ahead forecast 
  indepvar[84+i] <- pred[i]
}
pred


Tx <- data.matrix(base[,2])
MAPE(Tx[c(1,84)],y[c(1,84)])
MAPE(Tx[c(85:96)],pred[c(1:12)]) #Pour l'année 2017
MAPE(Tx[c(97:108)],pred[c(13:24)]) #Pour l'année 2018
MAPE(Tx[c(109:120)],pred[c(25:36)]) #Pour l'année 2019

rmse(y[c(107:118)],pred)
mse(y[c(107:118)],pred)
mae(y[c(107:118)],pred)

MAPE(y[c(107:118)],pred)   #library(MLmetrics)
mape(y[c(107:118)],pred)   #library Metrics


write(t(pred),file="forecast_en.txt",ncolumn=1,append=FALSE)