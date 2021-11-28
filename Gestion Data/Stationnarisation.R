library(readxl)
library(stringr)
library(xts)
library(fUnitRoots)
library(tsoutliers)

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
      df_diff<-cbind(df_diff,diff(df[,i],lag=1))
    }
    colnames(df_diff)<-Cname
  }
  return(df_diff)
}

base_diff <- Stationnarite(baseAll)
#les warning sont du au pvalue trop grande dans les ADF tests

#Verification de la stationnarite
Verif_Statio <- function(df){
  for (i in (3:length(df))){
    if(substr(colnames(df[i]),1,3) != "Tx_"){
      m1 <- ar(diff(df[,i]),method="mle")
      Test1 <- adfTest(df[,i], lag=m1$order)
      Test2 <- adfTest(df[,i], lag=m1$order, type="c")
      Test3 <- adfTest(df[,i], lag=m1$order, type="ct")
      if(Test1@test$p.value>0.1 | Test2@test$p.value>0.1 | Test3@test$p.value>0.1){
        text = str_c("La variable",colnames(df[i])," n'est pas bien stationnarisé, à verifier avec des graphiques")
        print(text)
      }
    }
  }
}

Verif_Statio(base_diff)

plot(base_diff$LCI)
plot(base_diff$Ita_coin)
plot(base_diff$Confiance_conso)
plot(base_diff$Confiance_ent)
plot(base_diff$USD_euro_change)
plot(base_diff$Qt_GT_credit_conso)

#Tout est ok, surtout que pour certaine la perte de stationnatite est dù à l'année 2020 qui est un peu mouvemente

#Outliers
df <- ts(base_diff[-c(120:131),3],start = c(2010,02), frequency=12)
fit <- tso(df)
plot(fit)
show(fit)
adj <- fit$yadj
#11 valeurs atyppiques qui sont modifie à l'aide de tso

adj <- as.data.frame(as.numeric(adj))
for (i in (1:12)){
  adj[nrow(adj)+1,] <- NA
}
base_diff$Tx_Defaut <- adj


write.csv(base_diff,"base_diff.csv")
