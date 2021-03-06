## Nowcasting one-step - recursive point-forecast with elastic net
library(readxl)
library(tidyverse)
library(lgarch)		# Gets modelling
library(gets)
library(glmnet)
library(Metrics)
library(MLmetrics)

setwd("C:/Users/33640/OneDrive/Bureau/DRIM")
dlbase <- read_excel("base_diff4.xlsx")
base<-dlbase
dlbase <- data.frame(dlbase[,-c(1,2,3)])
base2<- read_excel("Taux_Defaut_Projete.xlsx")

dlbase <- dlbase[-1,]
dlbase <- cbind(dlbase,base2[-120,4])
base2 <- base2[-1,]


#Change into numeric
for(i in (1:length(dlbase))){
  command = str_c("dlbase$",names(dlbase[i]),"<- as.numeric(dlbase$",names(dlbase[i]),")")
  eval(parse(text=command))
}
# Initialization
pred <- NULL

indepvar <- data.frame(base2) %>%
  select(Taux_Defaut_Projete) %>%
  #  scale(center = T, scale = T) %>%
  as.matrix()
depvar <- data.frame(dlbase) %>%
  #  scale(center = F, scale = F) %>%
  as.matrix()


# Nowcasting based on elastic-net
# One-step ahead forecasts for h=12
# forecast horizon h=12
for (i in 1:36) {
  x <- data.matrix(depvar[1:(82+i),])	
  y <- data.matrix(indepvar[1:(82+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  en_cv <- cv.glmnet(x, y, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cv <- en_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0.5, lambda = lambda_cv, standardize = T)
  pred <- c(pred, predict(model_cv, newx = depvar[(83+i),]))	# one-step ahead forecast 
  indepvar[83+i] <- pred[i]
  if(i!=36){depvar[84+i,63] <- pred[i]}
}
pred

model_cv$beta

Tx <- data.matrix(base2[,3])
MAPE(Tx[c(84:119)],pred) #Pour toute la periode
MAPE(Tx[c(84:95)],pred[c(1:12)]) #Pour l'ann�e 2017
MAPE(Tx[c(96:107)],pred[c(13:24)]) #Pour l'ann�e 2018
MAPE(Tx[c(108:119)],pred[c(25:36)]) #Pour l'ann�e 2019


rmse(y[c(107:118)],pred)
mse(y[c(107:118)],pred)
mae(y[c(107:118)],pred)

MAPE(y[c(107:118)],pred)   #library(MLmetrics)
mape(y[c(107:118)],pred)   #library Metrics

plot(pred)

library(xlsx)
write.xlsx(pred,"Prevision Taux Defaut Y/TauxDefautPrevision_Elastic.xlsx")
