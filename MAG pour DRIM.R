library(mgcv)
library(tidyverse)
library(MASS)
library(rmarkdown)
library(readxl)
library(stringr)
library(MLmetrics)
library(tidyverse)
library(glmnet)
library(Metrics)
library(MLmetrics)
library(stringr)

library(data.table)
library(car)
library(ggplot2)
library(grid)
library(animation)


# Importation des bases de donn�es =============================================
setwd("C:/Users/tibre/OneDrive/Bureau/DRIM")

  # Taux de d�faut, variable d'int�r�t
base <- read_xlsx("Taux_Defaut_Projete.xlsx")
Taux_defaut <- base[,-1]
View(base)

  # Variables explicatives diff�renci�es
var_exp <- read_xlsx("base_diff4.xlsx")
View(var_exp)
  ## on retire les colonnes inutiles
var_exp2 <- var_exp[,-c(1,2,3)]

  # Base compl�te
base_full <- cbind(Taux_defaut, var_exp2)
View(base_full)
  ## on cr�e une seconde base full o� on retire les lignes NA (1�re ligne de l'AR et derni�res lignes pour AR et Taux de d�faut)
base_full2 <- base_full[,-(4)]
base_full2 <- base_full
base_full2 <- base_full[-c(1,85:120),]
base_full2 <- base_full2[,-(4)]

View(base_full2)
str(base_full2)

base_full2$Date <- as.POSIXct(base_full2$Date, tz = "UTC", format = "%Y-%dT-%m")



# Recodage des variables explicatives ==========================================

str(base_full2)
for(i in (2:length(base_full2))){
  command = str_c("base_full2$",names(base_full2[i]),"<-
as.numeric(base_full2$",names(base_full2[i]),")")
  eval(parse(text=command))
}
  ## colonne date d�j� en format date


# Mod�lisation =================================================================

## Mod�le 1 : 10 premi�res variables -------------------------------------------
gam1 <- gam(Taux_Defaut_Projete ~ Tx_Chomage_M + Ita_coin + B_T_Climate + LCI +
            Tx_IPC+ Tx_interet_CT + Tx_interet_LT + Confiance_conso + Confiance_ent,
            data = base_full2, method = "REML")
summary(gam1)
  ## Tx_interet_CT � 0.1%
  ## Ita_coin � 5%
  ## Tx_IPC � 10%
MAPE(y_pred = exp(gam6$fitted.values) , y_true = data_train6$JOB_DURATION) 


## Mod�le 2 : AR + variables 11 � 20 -------------------------------------------
gam2 <- gam(Taux_Defaut_Projete ~ Ind_Inc_Pol_Eco + Qt_Euribor1 + Qt_Euribor2 +  Qt_FTSE + Qt_Enel +
            Qt_Eni + Qt_Exor + Qt_Generali + Tx_I_Deposits_E,
            data = base_full2, method = "REML")
summary(gam2)
  ## Qt_Eni, Qt_Generali, Tx_I_Deposits_E  � 0.1%
  ## Qt_Exor � 1%
  ## Qt_Euribor1, Qt_Enel � 5%
  ## Qt_Euribor2 � 10%

## Mod�le 3 : AR + variables 21 � 30 -------------------------------------------
gam3 <- gam(Taux_Defaut_Projete ~ Qt_Cours_actions + Tx_I_Loans + Tx_Bond_Yields_10 + USD_euro_change +
            Tx_Pret_Menage + Tx_Pret_SNF + BOP_Compte_Courant + BOP_Biens + BOP_Services,
            data = base_full2, method = "REML")
summary(gam3)
  ## Tx_I_Loans � 0.1%
  ## BOP_Compte_Couran � 5%
  ## Qt_Cours_actions, Tx_Bond_Yields_10 � 10%


## Mod�le 4 : AR + variables 31 � 40 -------------------------------------------
gam4 <- gam(Taux_Defaut_Projete ~ BOP_Capital + Qt_GT_Pret_bancaire + Qt_GT_credit_conso + Qt_GT_credit_immo +
            Qt_GT_taux_interet + Tx_bons_tresor + Qt_VIX + Qt_Telecom_ita + Qt_IXIC_NASDAQ,
            data = base_full2, method = "REML")
summary(gam4)
  ## Tx_bons_tresor,  Qt_Telecom_ita � 0.1%
  ## BOP_Capital, Qt_VIX � 5%


## Mod�le 5 : AR + variables 41 � 50 -------------------------------------------
gam5 <- gam(Taux_Defaut_Projete ~ Tx_Deposit_Resident + Tx_Interet_Deposit + Tx_Interet_Deposit_Over + Tx_Interet_Loans_House +
            Tx_Interet_Loans_Othe + Tx_Bon_Tresor_3 + Tx_Bon_Tresor_5 + Tx_Bon_Tresor_10 + Tx_Bon_Tresor_30,
            data = base_full2, method = "REML")
summary(gam5)
  ## Tx_Interet_Loans_House, Tx_Bon_Tresor_10 � 0.1%
  ## Tx_Interet_Deposit, Tx_Interet_Deposit_Over, Tx_Bon_Tresor_30 � 1%


## Mod�le 6 : AR + variables 51 � 60 -------------------------------------------
gam6 <- gam(Taux_Defaut_Projete ~ Qt_M1 + Qt_M2 + Qt_M3 + Tx_Fund_Raised +
            Tx_Refinancing + Tx_Debt_Securities + Tx_Total_Deposit + Tx_Loans_Other + Tx_Loans_Household,
            data = base_full2, method = "REML")
summary(gam6)
  ## AR � 0.1%
  ## Tx_Debt_Securities � 1%
  ## Tx_Refinancing � 5%


## Mod�le 7 : AR + variables 61 � la fin ---------------------------------------
gam7 <- gam(Taux_Defaut_Projete ~ AR + Tx_Loans_NonFinancial + Tx_Debt_CB + Qt_Debt_Monetary + Qt_Debt_Financial +
              Qt_Debt_Resident + Qt_Debt_NonResident + Qt_Debt_Gross + Qt_Stock_Gov,
            data = base_full2, method = "REML")
summary(gam7)
  ## AR � 0.1%
  ## Qt_Debt_Resident � 1%
  ## Qt_Debt_NonResident � 1%
  ## Qt_Debt_Financial � 5%
  ## Qt_Debt_Monetary � 10%
  ## Tx_Debt_CB � 10%
  ## Tx_Loans_NonFinancial � 10%

str(base_full2)


# Les variables les plus importantes sont :
## AR : 0.1%
## Tx_Chomage_M, Tx_interet_CT, Tx_I_Deposits_E, Tx_Debt_Securities, Qt_Debt_Resident, Qt_Debt_NonResident : 1%
## Qt_Eni, Tx_Bon_Tresor_3, Tx_Refinancing, Qt_Debt_Financial : 5%
## LCI, Tx_Bon_Tresor_30, Qt_Debt_Monetary, Tx_Debt_CB, Tx_Loans_NonFinancial : 10%


# Analyse graphiques des variables, on cherche les d�terminants avec des
# variations proches de celles du taux de d�faut projet� =======================

## Variables � 1%
par(mfrow=c(1,1))
par(mfrow=c(3,3))
plot(base_full2$Taux_Defaut_Projete)
plot(base_full2$AR)
plot(base_full2$Tx_Chomage_M)
plot(base_full2$Tx_interet_CT)
plot(base_full2$Tx_I_Deposits_E)
plot(base_full2$Tx_Debt_Securities, type = 'l')
plot(base_full2$Qt_Debt_Resident)
plot(base_full2$Qt_Debt_NonResident)
  # AR, Tx_interet_CT, Tx_Debt_Securities

## Variables � 5%
par(mfrow=c(2,3))
plot(base_full2$Taux_Defaut_Projete)
plot(base_full2$Qt_Eni)
plot(base_full2$Tx_Bon_Tresor_3)
plot(base_full2$Tx_Refinancing)
plot(base_full2$Qt_Debt_Financial)
  # Qt_Debt_Financial

## Variables � 10%
par(mfrow=c(2,3))
plot(base_full2$Taux_Defaut_Projete)
plot(base_full2$LCI)
plot(base_full2$Tx_Bon_Tresor_30)
plot(base_full2$Qt_Debt_Monetary)
plot(base_full2$Tx_Debt_CB)
plot(base_full2$Tx_Loans_NonFinancial)


# Mod�lisation � partir des variables significatives identifi�es ===============
GAM <- gam(Taux_Defaut_Projete ~ AR + Tx_Debt_Securities +
          Qt_Debt_Resident,
          data = base_full2, method = "REML")
summary(GAM)
MAPE(y_pred = GAM$fitted.values, y_true = base_full2$Taux_Defaut_Projete) # MAPE = 1,8% avec l'AR


# Mod�lisation sans l'AR =======================================================
GAM2 <- gam(Taux_Defaut_Projete ~ Tx_Debt_Securities + Tx_interet_CT + Qt_Debt_Financial +
             Qt_Debt_Resident,
           data = base_full2, method = "REML")
summary(GAM2)

par(mfrow=c(2,3))
plot(base_full2$Taux_Defaut_Projete)
plot(base_full2$Tx_Debt_Securities)
plot(base_full2$Tx_interet_CT)
plot(base_full2$Qt_Debt_Financial)
plot(base_full2$Qt_Debt_Resident)

## Ajout des splines sur les variables aux variations les plus �loign�es de la variable d'int�r�t 
GAM3 <- gam(Taux_Defaut_Projete ~ Tx_Debt_Securities + Tx_interet_CT + s(Qt_Debt_Financial, bs='cr') +
              Qt_Debt_Resident,
            data = base_full2, method = "REML")
summary(GAM3)

GAM4 <- gam(Taux_Defaut_Projete ~ Tx_Debt_Securities + Tx_interet_CT + s(Qt_Debt_Financial, bs='cr') +
            s(Qt_Debt_Resident, bs='cr'),
            data = base_full2, method = "REML")
summary(GAM4)

GAM5 <- gam(Taux_Defaut_Projete ~ Tx_Debt_Securities + s(Tx_interet_CT, bs='cr') +
            s(Qt_Debt_Resident, bs='cr'),
            data = base_full2, method = "REML")
summary(GAM5)
MAPE(y_pred = GAM5$fitted.values, y_true = base_full2$Taux_Defaut_Projete) # MAPE = 2,7%


par(mfrow=c(2,1))
plot(GAM5$fitted.values,  type = 'l')
plot(base_full2$Taux_Defaut_Projete,  type = 'l')

## les valeurs ajuct�es du mod�le GAM5 ont des variations tr�s proches de celles du taux de d�faut projet�


# On applique ce mod�le sur base_full, qui va jusqu'� la 120�me p�riode
# base_full_sansAR <- base_full[,-4]

GAM6 <- gam(Taux_Defaut_Projete ~ Tx_Debt_Securities + s(Tx_interet_CT, bs='cr') +
            s(Qt_Debt_Resident, bs='cr'),
            data = base_full, method = "REML")
summary(GAM6)
MAPE(y_pred = GAM5$fitted.values, y_true = base_full2$Taux_Defaut_Projete)


# Estimation des valeurs manquantes de la variable AR ==========================
View(base_full_bis)
## on retire la premi�re ligne, valeur d'AR manquante 
base_full_bis <- base_full[-1,]


## estimation en 2 �tapes : 
### 1) on estime les valeurs manquantes du taux de d�faut projet� via les mod�les GAM (avec AR) et GAM (sans AR)
### 2) on estime les valeurs manquantes de l'AR via les valeurs pr�dites du taux de d�faut projet�

base_full_bis <- base_full_bis[,-c(1:2)]

# Initialization
pred <- NULL

indepvar <- data.frame(base_full_bis) %>%
  select(Taux_Defaut_Projete) %>%
  #  scale(center = T, scale = T) %>%
  as.matrix()
depvar <- data.frame(base_full_bis) %>%
  select(-Taux_Defaut_Projete) %>% 
  #  scale(center = F, scale = F) %>%
  as.matrix()

# Nowcasting based on adaptive lasso
# One-step ahead forecasts for h=12
# forecast horizon h=12
for (i in 1:36) {
  x <- data.matrix(depvar[1:(83+i),])	
  y <- data.matrix(indepvar[1:(83+i)])
}

base_full_bis$AR[84] <- base_full_bis$Taux_Defaut_Projete[83]
pred <- NULL
newy
pred


for (i in 1:36) {
  newy <- predict(GAM,base_full_bis$Taux_Defaut_Projete[1:(84+i)])
  newx <- newy
}


plot(GAM5, shade = TRUE)

base_full_bis$TDP_sansAR <- base_full_bis$Taux_Defaut_Projete
View(base_full_bis$TDP_sansAR)

base_full_sansAR[84:119] <-

pred <- predict.gam(GAM5, base_full_bis$TDP_sansAR)

base_full_bis$Date <- as.POSIXct(base_full_bis$Date, tz = "UTC", format = "%Y-%dT-%m")
str(base_full_bis)



datas <- rbindlist(list(base_full_bis[, c("Taux_Defaut_Projete", "Date")],
                        data.table(value = GAM5$fitted.values,
                                   data_time = base_full_bis[, "Date"])))
datas <- datas[c(1:83),]
datas[, type := c(rep("Real", nrow(base_full_bis$Taux_Defaut_Projete)), rep("Fitted", nrow(base_full_bis[c(1:83),])))]

ggplot(data = base_full_bis, aes(Date, GAM5$fitted.values, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Taux de d�faut estim�",
       title = "valeurs estim�es de GAM5")



# plot estim� vs r�el, GAM5 ====================================================

datas$GAMfit <- GAM5$fitted.values

ggplot(datas, aes(Date)) + 
  geom_line(aes(y = Taux_Defaut_Projete, colour = "Taux de d�faut projet�")) + 
  geom_line(aes(y = GAMfit, colour = "Pr�dictions GAM5")) +
  theme_bw() +
  labs(x = "Temps", y = "Taux de d�faut",
       title = "Valeurs pr�dites par le GAM5, sans AR")

gam.check(GAM5)

vis.gam(GAM5, n.grid = 50, theta = 35, phi = 32, zlab = "",
        ticktype = "detailed", color = "topo")

# GAM ARIMA, AR(0), AR(1)

GAM5.AR.0 <- gamm(Taux_Defaut_Projete ~ Tx_Debt_Securities + s(Tx_interet_CT, bs='cr') +
                  s(Qt_Debt_Resident, bs='cr'),
                  data = base_full2,
                  method = "REML")

GAM5.AR.1 <- gamm(Taux_Defaut_Projete ~ Tx_Debt_Securities + s(Tx_interet_CT, bs='cr') +
               s(Qt_Debt_Resident, bs='cr'),
               data = base_full2,
               correlation = corARMA(form = ~ 1|Taux_Defaut_Projete, p = 1),
               method = "REML")

anova(GAM5.AR.0$lme, GAM5.AR.1$lme)




predict(GAM5, base_full_bis[c(85:119),])


# pr�diction des lignes 85 � 120 avec le GAM5 ==================================

str(base_full)
data_reg <- base_full[c("Taux_Defaut_Projete", "Tx_Debt_Securities", "Tx_interet_CT",
                        "Qt_Debt_Resident")]
View(data_reg)
str(data_reg)
data_reg <- as.data.frame(data_reg)
data_reg$Tx_interet_CT <- as.numeric(data_reg$Tx_interet_CT)
data_reg$Qt_Debt_Resident <- as.numeric(data_reg$Qt_Debt_Resident)
data_reg$Tx_Debt_Securities <- as.numeric(data_reg$Tx_Debt_Securities)


predGAM5 <- predict(GAM5, data_reg[c(85:120),])
View(data_reg.partie2) 

data_reg.partie1 <- data_reg[c(1:84),]
data_reg.partie2 <- data_reg[c(85:120),]
data_reg.partie2$Taux_Defaut_Projete <- predGAM5

data_reg <- rbind(data_reg.partie1, data_reg.partie2)
data_reg$Taux_Defaut_Historique <- base_full$Taux_Defaut_Historique
data_reg$Date <- base_full$Date
View(data_reg)

ggplot(data_reg, aes(Date)) + 
  geom_line(aes(y = Taux_Defaut_Projete, colour = "Taux de d�faut projet�")) + 
  geom_line(aes(y = Taux_Defaut_Historique, colour = "Taux_Defaut_Historique")) +
  theme_bw() +
  labs(x = "Temps", y = "Taux de d�faut",
       title = "Valeurs pr�dites par le GAM5, sans AR")

max(data_reg$Taux_Defaut_Projete)

plot(GAM5$fitted.values, type = "l")
plot(base_full$Taux_Defaut_Projete[c(85:120),])

ggplot(data_reg, aes(Date)) + 
  geom_line(aes(y = Tx_Debt_Securities , colour = "Tx_Debt_Securities")) + 
  geom_line(aes(y = Tx_interet_CT , colour = "Tx_interet_CT")) + 
  geom_line(aes(y = Qt_Debt_Resident, colour = "Qt_Debt_Resident")) +
  theme_bw() +
  labs(x = "Temps", y = "Taux de d�faut",
       title = "Variables explicatives")
str(data_reg)




