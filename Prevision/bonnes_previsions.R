
 

setwd("~/Master 2 EKAP/Drim_Game")


# libraries
library(readxl)
library(tidyverse)
library(dplyr)

library(lgarch)                # Gets modelling
library(gets)
library(ncvreg)
library(msaenet)
library(glmnet)         # penalized regressions
library(rbridge)        # bridge regressions
library(doParallel)        # execute foreach loops in parallel with %dopar% operator
# registerDoParallel(cores = 4)        # number of parallels (if nessary)


base <- read_excel("base_diff4.xlsx", col_types = c("numeric", 
                     "numeric", "numeric", "numeric", "numeric", 
                      "numeric", "numeric", "numeric", "numeric", 
                 "numeric", "numeric", "numeric", "numeric", 
                       "numeric", "numeric", "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "numeric", "numeric", 
                             "numeric", "numeric", "numeric", "numeric", 
                    "numeric", "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric"))


str(base)





base<-base[,-c(1:4)]

base1<-read_excel("Taux_Defaut_Projete.xlsx")

base<- cbind(base1,base)
base<-base[,-c(1:3,5)]

base<-ts(base)

#which(is.na(base),arr.ind=TRUE)  #voir les NA

#base <- data.frame(base) 
summary(base)
str(base)                # verify that all variables are numeric
training_base <- base
data.frame(training_base)



################ LASSO###################################

pred <- NULL

indepvar <- data.frame(training_base) %>%
  select(Taux_Defaut_Projete) %>%
  #  scale(center = T, scale = T) %>%
  as.matrix()
depvar <- data.frame(training_base) %>%
  select(-Taux_Defaut_Projete) %>% 
  #  scale(center = F, scale = F) %>%
  as.matrix()
# Nowcasting based on lasso
# One-step ahead forecasts for h=12
# forecast horizon h=12
for (i in 1:36) {
  x <- data.matrix(depvar[1:(83+i),])	
  y <- data.matrix(indepvar[1:(83+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cv <- lasso_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)
  pred <- c(pred, predict(model_cv, newx = depvar[(84+i),]))	# one-step ahead forecast 
  indepvar[84+i] <- pred[i]
}
pred
model_cv$beta
#Qt_Telecom_ita
#Qt_M3
#Tx_Debt_CB
library(Metrics)

library(MLmetrics)
 


base1<- base1[,-c(1,5)]
base2<- base1[85:120,]

base3<- cbind(base2,pred)
base3<- base3[,-3]

#là j'ai comparé le taux de défaut historique et les prévisions
Tx <- data.matrix(base3[,2])
MAPE(Tx[c(1:36)],pred[c(1:36)])                        #0.1502746    (0.1497774)
MAPE(Tx[c(1:12)],pred[c(1:12)]) #Pour l'année 2017     # 0.17
MAPE(Tx[c(13:24)],pred[c(13:24)]) #Pour l'année 2018    #0.18
MAPE(Tx[c(25:36)],pred[c(25:36)]) #Pour l'année 2019   # 0.088

rmse(Tx[c(1:36)],pred[c(1:36)]) #0.004794129
mse(Tx[c(1,36)],pred[c(1:36)])   # 2.298367e-05
mae(Tx[c(1,36)],pred[c(1:36)])    #0.0042989

x11()
plot(lasso_cv)


#voir les MAPE avec les variables non prédites, les vraies valeurs et les projettées

base8<- base1[1:84,]
Tx1 <- data.matrix(base8[,2])
Tx2 <- data.matrix(base8[,3])
MAPE(Tx1[c(1:84)],Tx2[c(1:84)])  #0.06529495 


base4<- base3[,c(2,3)]


library(ggplot2)
x11()
ggplot(base3)+ geom_line(aes(x=Date, y=pred), col="blue")+geom_line(aes(x=Date, y=Taux_Defaut_Historique), col="red")



#write.csv(base3,"base3.csv",row.names = FALSE)  #base pred
#write.csv(base8,"base8.csv",row.names = FALSE)  # base non pred


################################ A modifier #####################################
# Tx <- data.matrix(base3[,2])
# MAPE(Tx[c(1,84)],y[c(1,84)])
# MAPE(Tx[c(85:96)],pred[c(1:12)]) #Pour l'année 2017
# MAPE(Tx[c(97:108)],pred[c(13:24)]) #Pour l'année 2018
# MAPE(Tx[c(109:120)],pred[c(25:36)]) #Pour l'année 2019
# 
# rmse(y[c(107:118)],pred)
# mse(y[c(107:118)],pred)
# mae(y[c(107:118)],pred)
# 
# MAPE(y[c(107:118)],pred)   #library(MLmetrics)
# mape(y[c(107:118)],pred)   #library Metrics


########################################




##################### Ridge ###########################

pred<-NULL
for (i in 1:36) {
  x <- data.matrix(depvar[1:(83+i),])	
  y <- data.matrix(indepvar[1:(83+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cv <- ridge_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)
  pred <- c(pred, predict(model_cv, newx = depvar[(84+i),]))	# one-step ahead forecast 
  indepvar[84+i] <- pred[i]
}
pred


base3<- cbind(base2,pred)
base3<- base3[,-3]

Tx <- data.matrix(base3[,2])
MAPE(Tx[c(1:36)],pred[c(1:36)])                        #0.31 
MAPE(Tx[c(1:12)],pred[c(1:12)]) #Pour l'année 2017     # 0.26
MAPE(Tx[c(13:24)],pred[c(13:24)]) #Pour l'année 2018    #0.3190539
MAPE(Tx[c(25:36)],pred[c(25:36)]) #Pour l'année 2019   # 0.3724075


rmse(y[c(107:118)],pred)
mse(y[c(107:118)],pred)
mae(y[c(107:118)],pred)

MAPE(y[c(107:118)],pred)   #library(MLmetrics)
mape(y[c(107:118)],pred)   #library Metrics


ggplot(base3)+ geom_line(aes(x=Date, y=pred), col="blue")+geom_line(aes(x=Date, y=Taux_Defaut_Historique), col="red")

###################### Elastic Net ####################################

pred<- NULL
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

base3<- cbind(base2,pred)
base3<- base3[,-3]

Tx <- data.matrix(base3[,2])
MAPE(Tx[c(1:36)],pred[c(1:36)])                        #0.162861 
MAPE(Tx[c(1:12)],pred[c(1:12)]) #Pour l'année 2017     #0.1904393
MAPE(Tx[c(13:24)],pred[c(13:24)]) #Pour l'année 2018    #0.1859199
MAPE(Tx[c(25:36)],pred[c(25:36)]) #Pour l'année 2019   # 0.1122238


rmse(Tx[c(1:36)],pred[c(1:36)]) #0.006103136
mse(Tx[c(1,36)],pred[c(1:36)])   # 2.894195e-05
mae(Tx[c(1,36)],pred[c(1:36)])    #0.004839951

MAPE(y[c(107:118)],pred)   #library(MLmetrics)
mape(y[c(107:118)],pred)



ggplot(base3)+ geom_line(aes(x=Date, y=pred), col="blue")+geom_line(aes(x=Date, y=Taux_Defaut_Historique), col="red")
###########################################


########################## Ada Lasso ######################
pred<-NULL
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

base3<- cbind(base2,pred)
base3<- base3[,-3]

Tx <- data.matrix(base3[,2])
MAPE(Tx[c(1:36)],pred[c(1:36)])                        #0.1181244  
MAPE(Tx[c(1:12)],pred[c(1:12)]) #Pour l'année 2017     #0.1394006
MAPE(Tx[c(13:24)],pred[c(13:24)]) #Pour l'année 2018    #0.1260993
MAPE(Tx[c(25:36)],pred[c(25:36)])  #2019                #0.08887331

ggplot(base3)+ geom_line(aes(x=Date, y=pred), col="blue")+geom_line(aes(x=Date, y=Taux_Defaut_Historique), col="red")

########################################################



###########   SCAD ######################
pred<-NULL
for (i in 1:36) {
  x <- data.matrix(depvar[1:(83+i),])	
  y <- data.matrix(indepvar[1:(83+i)])
  
  cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"),nfolds=10)
  lambda_SCAD <- cvfit_SCAD$lambda.min
  model_cv=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
  pred <- c(pred, predict(model_cv, depvar[(84+i),]))	# one-step ahead forecast
  indepvar[84+i] <- pred[i]
}
pred

base3<- cbind(base2,pred)
base3<- base3[,-3]

Tx <- data.matrix(base3[,2])
MAPE(Tx[c(1:36)],pred[c(1:36)])                        #0.3734226  
MAPE(Tx[c(1:12)],pred[c(1:12)]) #Pour l'année 2017     #0.3927649
MAPE(Tx[c(13:24)],pred[c(13:24)]) #Pour l'année 2018    # 0.3287334
MAPE(Tx[c(25:36)],pred[c(25:36)])  #2019                #0.3987696



rmse(y[c(107:118)],pred)
mse(y[c(107:118)],pred)
mae(y[c(107:118)],pred)

MAPE(y[c(107:118)],pred)   #library(MLmetrics)
mape(y[c(107:118)],pred)   #library Metrics

##############################################################################





##########################recursiveMSAENET.R####################

pred<-NULL

#Prevision pour 2020
for (i in 1:36) {
  x <- data.matrix(depvar[1:(83+i),])	
  y <- data.matrix(indepvar[1:(83+i)])
  
  model_cv <- aenet(x, y, family = "gaussian", init = "ridge", alphas = 0.5, tune = "cv", nfolds = 10, seed = 1001)
  pred <- c(pred, predict(model_cv, newx = depvar[(84+i),]))	# one-step ahead forecast
  indepvar[84+i] <- pred[i]
}
pred

base3<- cbind(base2,pred)
base3<- base3[,-3]

Tx <- data.matrix(base3[,2])
MAPE(Tx[c(1:36)],pred[c(1:36)])                        #0.274233  
MAPE(Tx[c(1:12)],pred[c(1:12)]) #Pour l'année 2017     #0.2776058
MAPE(Tx[c(13:24)],pred[c(13:24)]) #Pour l'année 2018    #0.3391966
MAPE(Tx[c(25:36)],pred[c(25:36)])  #2019                #0.2058967


ggplot(base3)+ geom_line(aes(x=Date, y=pred), col="blue")+geom_line(aes(x=Date, y=Taux_Defaut_Historique), col="red")


