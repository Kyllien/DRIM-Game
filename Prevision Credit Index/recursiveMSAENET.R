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
    
    model_cv <- aenet(x, y, family = "gaussian", init = "ridge", alphas = 0.5, tune = "cv", nfolds = 10, seed = 1001)
    pred <- c(pred, predict(model_cv, newx = depvar[(83+i),]))	# one-step ahead forecast 
    indepvar[83+i] <- pred[i]
  }
  mo=cbind(mo,pred)
}
colnames(mo)<- c("Date","Ligne1","Ligne2","Ligne3","Ligne4","Ligne5","Ligne6","Ligne7","Ligne8","Ligne9","Ligne10")
library(xlsx)
write.xlsx(mo,"CreditIndexPredit_Msaenet.xlsx")

