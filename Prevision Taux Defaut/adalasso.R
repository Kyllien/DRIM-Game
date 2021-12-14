## Nowcasting one-step - recursive point-forecast with adaptive lasso
library(readxl)
library(tidyverse)
library(glmnet)
library(Metrics)
library(MLmetrics)
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

# Nowcasting based on adaptive lasso
# One-step ahead forecasts for h=12
# forecast horizon h=12
for (i in 1:36) {
  x <- data.matrix(depvar[1:(83+i),])	
  y <- data.matrix(indepvar[1:(83+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100
  lambda_cv <- lasso_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)
  coef_ridge <- predict(model_cv,type="coef",s=lambda_cv)
  gamma = 0.4
  w0 <- 1/(abs(coef_ridge) + (1/length(y)))
  poids.ridge <- w0^(gamma)
  poids.ridge <- poids.ridge[2:nrow(poids.ridge),]
  poids.ridge <- Matrix(poids.ridge)
  fit_adalasso <- glmnet(x, y, penalty.factor =poids.ridge)
  fit_cv_adalasso <- cv.glmnet(x, y,penalty.factor=poids.ridge)
  lambda_cv <- fit_cv_adalasso$lambda.1se
  model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)
  pred <- c(pred, predict(model_cv, newx = depvar[(84+i),]))	# one-step ahead forecast 
  indepvar[84+i] <- pred[i]
}
pred
model_cv$beta

Tx <- data.matrix(base2[,3])
MAPE(Tx[c(1:84)],indepvar[c(1:84)])
MAPE(Tx[c(85:120)],pred)
MAPE(Tx[c(85:96)],pred[c(1:12)]) #Pour l'année 2017
MAPE(Tx[c(97:108)],pred[c(13:24)]) #Pour l'année 2018
MAPE(Tx[c(109:120)],pred[c(25:36)]) #Pour l'année 2019


rmse(y[c(107:118)],pred)
mse(y[c(107:118)],pred)
mae(y[c(107:118)],pred)

MAPE(y[c(107:118)],pred)   #library(MLmetrics)
mape(y[c(107:118)],pred)   #library Metrics

plot(pred)

library(xlsx)
write.xlsx(pred,"Prevision Taux Defaut/TauxDefautPrevision_AdaLasso.xlsx")
