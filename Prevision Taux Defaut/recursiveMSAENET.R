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
dlbase <- data.frame(dlbase[,-c(1,2,3)])
base2<- read_excel("Taux_Defaut_Projete.xlsx")

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

for (i in 1:36) {
  x <- data.matrix(depvar[1:(83+i),])	
  y <- data.matrix(indepvar[1:(83+i)])
  
  model_cv <- aenet(x, y, family = "gaussian", init = "ridge", alphas = 0.5, tune = "cv", nfolds = 10, seed = 1001)
  pred <- c(pred, predict(model_cv, newx = depvar[(84+i),]))	# one-step ahead forecast
  indepvar[84+i] <- pred[i]
}
pred

Tx <- data.matrix(base[,3])
MAPE(Tx[c(1:84)],y[c(1:84)])
MAPE(Tx[c(85:120)],pred) #Pour toute la periode
MAPE(Tx[c(85:96)],pred[c(1:12)]) #Pour l'année 2017
MAPE(Tx[c(97:108)],pred[c(13:24)]) #Pour l'année 2018
MAPE(Tx[c(109:120)],pred[c(25:36)]) #Pour l'année 2019



plot(pred)

library(xlsx)
write.xlsx(pred,"Prevision Taux Defaut/TauxDefautPrevision_Msaenet.xlsx")

