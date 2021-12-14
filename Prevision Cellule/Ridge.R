


DetachPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0) {
    for (package in package.list) {
      detach(package, character.only=TRUE)
    }
  }
}
DetachPackages()


## Nowcasting one-step - recursive point-forecast with ridge
library(xlsx)
library(readxl)
library(tidyverse)
library(glmnet)		
library(Metrics)
library(MLmetrics)

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
      ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
      lambda_cv <- ridge_cv$lambda.1se
      model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)      
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
  write.xlsx(base3, "Cellule_PIT/PredictionCellule_Ridge.xlsx", sheetName = sheet, 
             col.names = TRUE, row.names = TRUE, append = TRUE)
  print(i)
}

