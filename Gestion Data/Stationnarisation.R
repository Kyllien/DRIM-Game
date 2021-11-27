library(readxl)
library(stringr)
library(xts)
library(fUnitRoots)

setwd("C:/Users/33640/OneDrive/Bureau/DRIM")
base1 <- read_xlsx("base_Tx1.xlsx",sheet='Data par mois')
base2 <- read_xlsx("base_Tx2.xlsx")

#Mise en place des types
baseAll <- cbind(base1,base2[,c(2:27)])
for (i in (3:length(baseAll))){
  command <- str_c("baseAll$",colnames(baseAll[i])," <- as.numeric(baseAll$",colnames(baseAll[i]),")")
  eval(parse(text = command))
}
baseAll$Date <- as.Date(baseAll$Date)

Stationnarite <- function(df){
  #On met les deux premiere colonnes dans la seconde base car cest la variable à expliquer et la date
  df_diff <- df[-1,c(1,2)]
  for (i in (3:length(df))){
    Cname <- colnames(df_diff)
    Cname <-append(Cname,colnames(df[i]))
    if(substr(colnames(df[i]),1,3) == "Tx_"){
      df_diff<-cbind(df_diff,df[-1,i])
    }
    else if(substr(colnames(df[i]),1,3) == "Qt_"){
      m1 <- ar(diff(df[,i]),method="mle")
      Test1 <- adfTest(df[,i], lag=m1$order)
      Test2 <- adfTest(df[,i], lag=m1$order, type="c")
      Test3 <- adfTest(df[,i], lag=m1$order, type="ct")
      if(Test1@test$p.value>0.05 | Test2@test$p.value>0.05 | Test3@test$p.value>0.05){
        df_diff<-cbind(df_diff,diff(log(df[,i]),lag=1))
      }
      else{
        df_diff<-cbind(df_diff,log(df[-1,i]))
      }
    }
    else{
      df_diff<-cbind(df_diff,log(df[-1,i]))
    }
    colnames(df_diff)<-Cname
  }
  return(df_diff)
}