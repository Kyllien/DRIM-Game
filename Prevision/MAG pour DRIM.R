library(mgcv)
library(tidyverse)
library(MASS)
library(rmarkdown)
library(readxl)
library(stringr)
library(MLmetrics)




# Importation des bases de donn�es =============================================
setwd("C:/Users/tibre/OneDrive/Bureau/DRIM")

  # Taux de d�faut, variable d'int�r�t
base <- read_xlsx("Taux_Defaut_Projete.xlsx")
Taux_defaut <- base[,-1]
View(Taux_defaut)

  # Variables explicatives diff�renci�es
var_exp <- read_xlsx("base_diff4.xlsx")
View(var_exp2)
  ## on retire les colonnes inutiles
var_exp2 <- var_exp[,-c(1,2,4)]

  # Base compl�te
base_full <- cbind(Taux_defaut, var_exp2)
View(base_full)
  ## on cr�e une seconde base full o� on retire les lignes NA (1�re ligne de l'AR et derni�res lignes pour AR et Taux de d�faut)
base_full2 <- base_full[-c(1,85:120),]
base_full2 <- base_full2[,-(5)]
View(base_full2)


# Recodage des variables explicatives ==========================================
str(base_full2)
for(i in (1:length(base_full2))){
  command = str_c("base_full2$",names(base_full2[i]),"<-
as.numeric(base_full2$",names(base_full2[i]),")")
  eval(parse(text=command))
}
  ## colonne date d�j� en format date


# Mod�lisation =================================================================

## Mod�le 1 : 10 premi�res variables -------------------------------------------
gam1 <- gam(Taux_Defaut_Projete ~ AR + Tx_Chomage_M + Ita_coin + B_T_Climate + LCI +
            Tx_IPC+ Tx_interet_CT + Tx_interet_LT + Confiance_conso + Confiance_ent,
            data = base_full2, method = "REML")
summary(gam1)
  ## AR � 0.1%
  ## Tx_Chomage_M � 0.1%
  ## Tx_interet_CT � 1%
  ## LCI � 10%
MAPE(y_pred = exp(gam6$fitted.values) , y_true = data_train6$JOB_DURATION) 


## Mod�le 2 : AR + variables 11 � 20 -------------------------------------------
gam2 <- gam(Taux_Defaut_Projete ~ AR + Ind_Inc_Pol_Eco + Qt_Euribor1 + Qt_Euribor2 +  Qt_FTSE + Qt_Enel +
            Qt_Eni + Qt_Exor + Qt_Generali + Tx_I_Deposits_E,
            data = base_full2, method = "REML")
summary(gam2)
  ## AR � 0.1%
  ## Tx_I_Deposits_E � 1%
  ## Qt_Eni � 5%


## Mod�le 3 : AR + variables 21 � 30 -------------------------------------------
gam3 <- gam(Taux_Defaut_Projete ~ AR + Qt_Cours_actions + Tx_I_Loans + Tx_Bond_Yields_10 + USD_euro_change +
            Tx_Pret_Menage + Tx_Pret_SNF + BOP_Compte_Courant + BOP_Biens + BOP_Services,
            data = base_full2, method = "REML")
summary(gam3)
  ## AR � 0.1%


## Mod�le 4 : AR + variables 31 � 40 -------------------------------------------
gam4 <- gam(Taux_Defaut_Projete ~ AR + BOP_Capital + Qt_GT_Pret_bancaire + Qt_GT_credit_conso + Qt_GT_credit_immo +
            Qt_GT_taux_interet + Tx_bons_tresor + Qt_VIX + Qt_Telecom_ita + Qt_IXIC_NASDAQ,
            data = base_full2, method = "REML")
summary(gam4)
  ## AR � 0.1%


## Mod�le 5 : AR + variables 41 � 50 -------------------------------------------
gam5 <- gam(Taux_Defaut_Projete ~ AR + Tx_Deposit_Resident + Tx_Interet_Deposit + Tx_Interet_Deposit_Over + Tx_Interet_Loans_House +
            Tx_Interet_Loans_Othe + Tx_Bon_Tresor_3 + Tx_Bon_Tresor_5 + Tx_Bon_Tresor_10 + Tx_Bon_Tresor_30,
            data = base_full2, method = "REML")
summary(gam5)
  ## AR � 0.1%
  ## Tx_Bon_Tresor_3 � 5%
  ## Tx_Bon_Tresor_30 � 10%


## Mod�le 6 : AR + variables 51 � 60 -------------------------------------------
gam6 <- gam(Taux_Defaut_Projete ~ AR + Qt_M1 + Qt_M2 + Qt_M3 + Tx_Fund_Raised +
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
par(mfrow=c(3,3))
plot(base_full2$Taux_Defaut_Projete)
plot(base_full2$AR)
plot(base_full2$Tx_Chomage_M)
plot(base_full2$Tx_interet_CT)
plot(base_full2$Tx_I_Deposits_E)
plot(base_full2$Tx_Debt_Securities)
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
MAPE(y_pred = exp(GAM$fitted.values), y_true = base_full2$Taux_Defaut_Projete) 


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
plot(GAM$fitted.values)
plot(base_full2$Taux_Defaut_Projete)
## les valeurs ajuct�es du mod�le GAM5 ont des variations tr�s proches de celles du taux de d�faut projet�


# On applique ce mod�le sur base_full, qui va jusqu'� la 120�me p�riode
base_full_sansAR <- base_full[,-4]
GAM6 <- gam(Taux_Defaut_Projete ~ Tx_Debt_Securities + s(Tx_interet_CT, bs='cr') +
            s(Qt_Debt_Resident, bs='cr'),
            data = base_full_sansAR, method = "REML")
summary(GAM6)
MAPE(y_pred = GAM5$fitted.values, y_true = base_full2$Taux_Defaut_Projete)










