## Nowcasting one-step - recursive point-forecast with adaptive lasso
library(readxl)
library(tidyverse)
library(glmnet)
library(Metrics)
library(MLmetrics)
library(stringr)

setwd("C:/Users/33640/OneDrive/Bureau/DRIM")
dlbase <- read_excel("base_diff4.xlsx")
dlbase <- data.frame(dlbase[,-c(1,2,3)])

base2 <- read_excel("CREDIT SCORE INDEX.xlsx")
colnames(base2)<- c("Date","Ligne1","Ligne2","Ligne3","Ligne4","Ligne5","Ligne6","Ligne7","Ligne8","Ligne9","Ligne10")
base2<-base2[-84,]

#Change into numeric
for(i in (1:length(dlbase))){
  command = str_c("dlbase$",names(dlbase[i]),"<- as.numeric(dlbase$",names(dlbase[i]),")")
  eval(parse(text=command))
}

mo=as.data.frame(1:37)
colnames(mo)="Date"
for (j in c("Ligne1","Ligne2","Ligne3","Ligne4","Ligne5","Ligne6","Ligne7","Ligne8","Ligne9","Ligne10")){
  # Initialization
  pred <- NULL
  
  command <- str_c("indepvar <- data.frame(base2) %>%
    select(",j,") %>%
    as.matrix()")
  eval(parse(text=command))
  depvar <- data.frame(dlbase) %>%
    #  scale(center = F, scale = F) %>%
    as.matrix()
  
  # Nowcasting based on adaptive lasso
  # One-step ahead forecasts for h=37
  # forecast horizon h=37
  for (i in 1:37) {
    x <- data.matrix(depvar[1:(82+i),])	
    y <- data.matrix(indepvar[1:(82+i)])
    
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
    pred <- c(pred, predict(model_cv, newx = depvar[(83+i),]))	# one-step ahead forecast 
    indepvar[83+i] <- pred[i]
  }
  mo=cbind(mo,pred)
}
colnames(mo)<- c("Date","Ligne1","Ligne2","Ligne3","Ligne4","Ligne5","Ligne6","Ligne7","Ligne8","Ligne9","Ligne10")
library(xlsx)
write.xlsx(mo,"CreditIndexPredit_AdaLasso.xlsx")
