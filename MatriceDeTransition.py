import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import transitionMatrix as tm
from transitionMatrix.estimators import simple_estimator as es
from transitionMatrix.estimators import cohort_estimator as ec
import datetime

#data = pd.read_csv('C:/Users/33640/OneDrive/Bureau/DRIM/DRiM 2021/base_1.csv',index_col='dtf_per_trt', parse_dates=True)
data = pd.read_csv('C:/Users/33640/OneDrive/Bureau/DRIM/DRiM 2021/base_1.csv')

# print(np.unique(data['classe'])) #Aucun prob de classe

#rename the columns
data = data.rename(columns={"classe": "State", "dtf_per_trt": "Time", "ID_BCR_TRS": "ID"})
# Put Time in Date Forma
data["Time"]=pd.to_datetime(data["Time"])

#Creation du Jeu Test
#df = data.sample(1000000)
#df.to_csv('C:/Users/33640/source/repos/Kyllien/DRIM-Game/data_sample.csv')

df = data.head(1000000)
df = df.sort_values(by=["ID","Time"])

#Mise en place des notes pour Transition Matrix
description = [('1', "Grade 1"), ('2', "Grade 2"), ('3', "Grade 3"),
               ('4', "Grade 4"), ('5', "Grade 5"), ('6', "Grade 6"),
               ('7', "Grade 7"), ('8', "Grade 8"), ('9', "Grade 9"),
               ('10', "Grade 10"),('11', "Default")]
myState = tm.StateSpace(description)

#Creation de la fonction pour avoir une data par type d'intervalle de temps 
#Creation d'un dico pour chaque matrice de transition, avec choix de l'intervalle : "Y","Q","M"
## A FAIRE
def CreationDataParDate(df,interval) : 
    for i in np.unique(df['State']) :
        print(i)
        

    return df2

#Fonction qui nous sort les IN et OUT pour un seul ID
def CreationInOut(df,id):
    In = df["State"].iloc[0]
    Out = df["State"].iloc[-1]
    return In, Out

#Fonction qui sort la data des IN et OUT pour un DF par ID, besoin de CreationInOut pour tourner
def CreationDataInOut(df):
    df2 = []
    for i in np.unique(df['ID']) :
        df3 = df[df["ID"]==i]
        In, Out = CreationInOut(df3,i)
        df2.append([i,In,Out])
    df4 = pd.DataFrame(df2, columns=['ID', 'IN', 'OUT'])
    return(df4)

data2 = CreationDataInOut(data)

#Fonction permettant la creation d'une matrice de transition avce seulement les IN et OUT pour chaque ID
#Les plus de cette methode : 
#Working with real data limitations 
#Alternative cohort estimators 
#Correcting estimated matrices
#POint negatif : 
#The data are cohorted in very uneven time intervals
#Borrowers cannot be tracked from cohort to cohort
#There is a reported initial credit grade assignment but not a final one (for current borrowers)

##BUG BUG BUG
data2 = CreationDataInOut(data)
def TransitionMatrixInOut (df) :
    labels = {'State': 'IN'}
    print(myState.validate_dataset(dataset=df4, labels=labels))
    labels = {'State': 'OUT'}
    print(myState.validate_dataset(dataset=df4, labels=labels))
    myEstimator = es.SimpleEstimator(states=myState, ci={'method': 'goodman', 'alpha': 0.05})
    result = myEstimator.fit(df4)
    myEstimator.summary()
    myMatrix = tm.TransitionMatrixSet(values=result, temporal_type='Incremental')
    print(myMatrix.temporal_type)
    myMatrix.print_matrix()
    return myMatrix 

#Fonction qui permet d'obtenir le TimeStep, c'est a dire obtenir le rang de la date, ex : janvier 2010 =0 et fevrier2010=1 si par mois
#si par an : 2010 = 0 et 2011 = 1 etc
#interval = 'Y' ou 'M' ou 'Q' ou 'TTC'
def CreationTimestep(df,interval) :
    if(interval=='Y'):
        d={}
        c=0
        for i in np.unique(df["Time"].dt.year) :
            d[c]=df[df["Time"].dt.year==i]
            d[c]["Timestep"] = c
            c=c+1
        #A TROUVER : UNE MEILLEURE FACON DE TRANSFORMER LE DICO EN DF !!!!!!!!!!!!!!!!!!!!!
        df2=d[0]
        for i in range(1,c) :
            df2=np.concatenate((df2,d[i]),axis=0)
        df3 = pd.DataFrame(df2,columns=["ID","Time","State","Timestep"])
        df3 = df3.sort_values(by=["ID","Time"])
    
    elif(interval=='M') :
        d={}
        c=0
        for i in np.unique(df["Time"].dt.year) :
            df2=df[df["Time"].dt.year==i]
            for j in np.unique(df["Time"].dt.month) :
                d[c]=df2[df2["Time"].dt.month==j]
                d[c]["Timestep"] = c
                c=c+1
        #A TROUVER : UNE MEILLEURE FACON DE TRANSFORMER LE DICO EN DF !!!!!!!!!!!!!!!!!!!!!
        df2=d[0]
        for i in range(1,c) :
            df2=np.concatenate((df2,d[i]),axis=0)
        df3 = pd.DataFrame(df2,columns=["ID","Time","State","Timestep"])
        df3 = df3.sort_values(by=["ID","Time"])

    #A TROUVER :  UNE MEILLEURE FACON DE FAIRE POUR LES TRIMESTRES !!!! A FAIRE BUGBUGBUG
    #elif(interval=='Q') :
    #    d={}
    #    c=0
    #    for i in np.unique(df["Time"].dt.year) :
    #        #df2=df[df["Time"].dt.year==i]
    #        print("i : ",i)
    #        c2=0
    #        for j in range (1,5) :
    #            print("c : ",c, "i : ",i)
    #            print(c-(c2*12)+1)
    #            c=c+1
    #            print(c-(c2*12)+1)
    #            c=c+1
    #            print(c-(c2*12)+1)
    #            print(c2)
    #            #
    #            #d[c]=df2[df2["time"].dt.month==j]
    #            #d[c]=df2[df2["time"].dt.month==j]
    #            #d[c]["timestep"] = c
    #            c=c+1
    #        c2=c2+1


    #    #A TROUVER : UNE MEILLEURE FACON DE TRANSFORMER LE DICO EN DF !!!!!!!!!!!!!!!!!!!!!
    #    df2=d[0]
    #    for i in range(1,c) :
    #        df2=np.concatenate((df2,d[i]),axis=0)
    #    df3 = pd.DataFrame(df2,columns=["ID","Time","State","Timestep"])
    #    df3 = df3.sort_values(by=["ID","Time"])



    else :
        print("Mauvais Intervalle (Y ou Q ou M), reflechis pelo")
    return df3

data = data.sort_values(by=["ID","Time"])
data2=CreationTimestep(data,'M')

#Fonction qui permet la mise en place d'une matrice a partir de la technique des cohortes
def TransitionMatrixCohort (df) :
    myEstimator = ec.CohortEstimator(states=myState, ci={'method': 'goodman', 'alpha': 0.05})
    result = myEstimator.fit(data2)
    myMatrixSet = tm.TransitionMatrixSet(values=result, temporal_type='Incremental')
    myEstimator.summary()
    return myMatrix