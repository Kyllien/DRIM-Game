# DRIM-Game
Tout le code et les documents nécessaires à la résolution du DRIM Game 2021.
 
Le sujet est le suivant : "Détermination d’un modèle de projection de matrice de migration dans un portefeuille de crédits."

L’objectif du sujet est de déterminer une méthode de projection alternative de matrice de migration et d’en évaluer la capacité prédictive au travers d’un backtesting.

Fonction matrice de corrélation :

var.signi <- base_full2[,c("Qt_Eni","Tx_I_Deposits_E","Tx_I_Loans","Tx_bons_tresor","Qt_Telecom_ita","Tx_Interet_Loans_House",
                           "Tx_Bon_Tresor_10","Tx_Loans_Household","Tx_Debt_Securities","Tx_interet_CT","Qt_Debt_Financial","Qt_Debt_Resident")]

library("PerformanceAnalytics")
chart.Correlation(var.signi, histogram=FALSE, pch=19)
