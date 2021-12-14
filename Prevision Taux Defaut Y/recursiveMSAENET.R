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

for (i in 1:36) {
  x <- data.matrix(depvar[1:(82+i),])	
  y <- data.matrix(indepvar[1:(82+i)])
  
  model_cv <- aenet(x, y, family = "gaussian", init = "ridge", alphas = 0.5, tune = "cv", nfolds = 10, seed = 1001)
  pred <- c(pred, predict(model_cv, newx = depvar[(83+i),]))	# one-step ahead forecast
  indepvar[83+i] <- pred[i]
  if(i!=36){depvar[84+i,63] <- pred[i]}
}
pred

model_cv$beta

Tx <- data.matrix(base2[,3])
MAPE(Tx[c(84:119)],pred) #Pour toute la periode
MAPE(Tx[c(84:95)],pred[c(1:12)]) #Pour l'année 2017
MAPE(Tx[c(96:107)],pred[c(13:24)]) #Pour l'année 2018
MAPE(Tx[c(108:119)],pred[c(25:36)]) #Pour l'année 2019



plot(pred)

library(xlsx)
write.xlsx(pred,"Prevision Taux Defaut Y/TauxDefautPrevision_Msaenet.xlsx")

