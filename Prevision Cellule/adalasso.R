## Nowcasting one-step - recursive point-forecast with adaptive lasso
library(readxl)
library(tidyverse)
library(glmnet)
library(Metrics)
library(MLmetrics)
library(stringr)
library(xlsx)

setwd("C:/Users/33640/OneDrive/Bureau/DRIM")
dlbase <- read_excel("base_diff4.xlsx")
dlbase <- data.frame(dlbase[,-c(1,2,3)])

#Change into numeric
for(k in (1:length(dlbase))){
  command = str_c("dlbase$",names(dlbase[k]),"<- as.numeric(dlbase$",names(dlbase[k]),")")
  eval(parse(text=command))
}

for (i in 1:10){
  command <- str_c("base2 <- read_excel('Cellule_PIT/Cellule_Classe_",i,".xlsx')")
  eval(parse(text=command))
  base2 <- base2[-84,]
  base <- data.frame
  
  for(k in 3:12){
  # Initialization
  pred <- NULL
  
  indepvar <- data.frame(base2) %>%
    select(k) %>%
    #  scale(center = T, scale = T) %>%
    as.matrix()
  depvar <- data.frame(dlbase) %>%
    #  scale(center = F, scale = F) %>%
    as.matrix()
  
  # Nowcasting based on adaptive lasso
  # One-step ahead forecasts for h=12
  # forecast horizon h=12
  for (j in 1:37) {
    x <- data.matrix(depvar[1:(82+j),])	
    y <- data.matrix(indepvar[1:(82+j)])
    
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
    pred <- c(pred, predict(model_cv, newx = depvar[(83+j),]))	# one-step ahead forecast 
    indepvar[83+j] <- pred[j]
  }
  pred
  base <- cbind(base,pred)
  print(k)
  }
  base3 <- base[,-1]
  sheet <- str_c("Classe",i)
  colnames(base3) <- c("Ligne_1","Ligne_2","Ligne_3","Ligne_4","Ligne_5","Ligne_6","Ligne_7","Ligne_8","Ligne_9","Ligne_10")
  write.xlsx(base3, "Cellule_PIT/PredictionCellule_AdaLasso.xlsx", sheetName = sheet, 
             col.names = TRUE, row.names = TRUE, append = TRUE)
  print(i)
}

