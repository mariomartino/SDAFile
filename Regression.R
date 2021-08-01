# Script definitivo per la Regression

# TRAINING 

# Caricamento Dataset per l'addestramento

setwd('/Users/marti/Documents/Mario/LEZIONI Università/Magistrale/Statistical Data Analysis/Progetto')
myds=read.csv('RegressionData_SDA_IZ_group2.csv')

setwd('Inserire PATH')
myds = read.csv('Inserire FILENAME')

# Addestramento definitivo sistemi 



# PREDICT

Xnew = data.frame("X_Temperature" = VALUE, "X_Humidity" = VALUE, "X_Altitude" = VALUE, "X_ClimaticConditions" = VALUE, "X_RestTimeFromLastMatch" = VALUE, "X_AvgPlayerValue" = VALUE, "X_MatchRelevance" = VALUE, "X_AvgGoalConcededLastMatches" = VALUE, "X_SupportersImpact" = VALUE, "X_OpposingSupportersImpact" = VALUE)



Ynew = data.frame("Y_Dehydration" = Y_Dehydration, "Y_Hyperthermia" = Y_Hyperthermia, "Y_AvgSpeed" = Y_AvgSpeed, "Y_AvgTravelledDistance" = Y_AvgTravelledDistance, "Y_PressingCapability" = Y_PressingCapability, "Y_PhysicalEndurance" = Y_PhysicalEndurance, "Y_MentalConcentration" = Y_MentalConcentration, "Y_EmotionalMotivation" = Y_EmotionalMotivation)