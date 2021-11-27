## Nowcasting one-step - recursive point-forecast with msaenet package
library(readxl)
library(tidyverse)
library(glmnet)
library(msaenet)
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

# Nowcasting based on adaptive elastic-net
# One-step ahead forecasts for h=12
# forecast horizon h=12
for (i in 1:12) {
  x <- data.matrix(depvar[1:(106+i),])	
  y <- data.matrix(indepvar[1:(106+i)])
  
  model_cv <- aenet(x, y, family = "gaussian", init = "ridge", alphas = 0.5, tune = "cv", nfolds = 10, seed = 1001)
  pred <- c(pred, predict(model_cv, newx = depvar[(107+i),]))	# one-step ahead forecast 
}
pred


rmse(y[c(107:118)],pred)
mse(y[c(107:118)],pred)
mae(y[c(107:118)],pred)

MAPE(y[c(107:118)],pred)   #library(MLmetrics)
mape(y[c(107:118)],pred)   #library Metrics

write(t(pred),file="forecast_aenet.txt",ncolumn=1,append=FALSE)

