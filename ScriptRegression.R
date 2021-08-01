
setwd('/Users/marti/Documents/Mario/LEZIONI Università/Magistrale/Statistical Data Analysis/Progetto')
myds=read.csv('RegressionData_SDA_IZ_group2.csv')

# Defining functions we'll use for our project

mse_func=function(actual, predicted)
{
  mean( (actual-predicted)^2 )
}

# Splitting the Training Set

divTrainTest = 0.8*dim(myds)[1]
Train = myds[1:divTrainTest,]
Test = myds[(divTrainTest+1):dim(myds)[1],]

X_train = Train[,1:10]
Y_train = Train[,11:18]
X_test = Test[,1:10]
Y_test = Test[,11:18]

# Multiple Linear Regression

RegDehydration = lm(Y_Dehydration ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train)
RegHyperthermia = lm(Y_Hyperthermia ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train)
RegAvgSpeed = lm(Y_AvgSpeed ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train)
RegAvgTravelledDistance = lm(Y_AvgTravelledDistance ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train)
RegPressingCapability = lm(Y_PressingCapability ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train)
RegPhysicalEndurance = lm(Y_PhysicalEndurance ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train)
RegMentalConcentration = lm(Y_MentalConcentration ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train)
RegEmotionalMotivation = lm(Y_EmotionalMotivation ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train)

summary(RegDehydration)
summary(RegHyperthermia)
summary(RegAvgSpeed)
summary(RegAvgTravelledDistance)
summary(RegPressingCapability)
summary(RegPhysicalEndurance)
summary(RegMentalConcentration)
summary(RegEmotionalMotivation)

mseTest_Dehydration = mse_func(Test$Y_Dehydration, predict(RegDehydration, Test))

mseTest_Hyperthermia = mse_func(Test$Y_Hyperthermia, predict(RegHyperthermia, Test))

mseTest_AvgSpeed = mse_func(Test$Y_AvgSpeed, predict(RegAvgSpeed, Test))

mseTest_AvgTravelledDistance = mse_func(Test$Y_AvgTravelledDistance, predict(RegAvgTravelledDistance, Test))

mseTest_PressingCapability = mse_func(Test$Y_PressingCapability, predict(RegPressingCapability, Test))

mseTest_PhysicalEndurance = mse_func(Test$Y_PhysicalEndurance, predict(RegPhysicalEndurance, Test))

mseTest_MentalConcentration = mse_func(Test$Y_MentalConcentration, predict(RegMentalConcentration, Test))

mseTest_EmotionalMotivation = mse_func(Test$Y_EmotionalMotivation, predict(RegEmotionalMotivation, Test))

mlrMse = c(mseTest_Dehydration, mseTest_Hyperthermia, mseTest_AvgSpeed, mseTest_AvgTravelledDistance, mseTest_PhysicalEndurance, mseTest_MentalConcentration, mseTest_EmotionalMotivation, mseTest_PressingCapability)

# Ridge and Lasso subset selection

library(glmnet)

# Trying to find out the best Lambda value for a Ridge Model

lambdaAttempts = 10^seq(-3,10,length=50)

Ridge_Dehydration = glmnet(X_train, Y_train$Y_Dehydration, alpha=0, lambda=lambdaAttempts)
Ridge_Hyperthermia = glmnet(X_train, Y_train$Y_Hyperthermia, alpha=0, lambda=lambdaAttempts)
Ridge_AvgSpeed = glmnet(X_train, Y_train$Y_AvgSpeed, alpha=0, lambda=lambdaAttempts)
Ridge_AvgTravelledDistance = glmnet(X_train, Y_train$Y_AvgTravelledDistance, alpha=0, lambda=lambdaAttempts)
Ridge_PressingCapability = glmnet(X_train, Y_train$Y_PressingCapability, alpha=0, lambda=lambdaAttempts)
Ridge_PhysicalEndurance = glmnet(X_train, Y_train$Y_PhysicalEndurance, alpha=0, lambda=lambdaAttempts)
Ridge_MentalConcentration = glmnet(X_train, Y_train$Y_MentalConcentration, alpha=0, lambda=lambdaAttempts)
Ridge_EmotionalMotivation = glmnet(X_train, Y_train$Y_EmotionalMotivation, alpha=0, lambda=lambdaAttempts)

u = c()
temp = c()

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_Dehydration, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_Dehydration, temp))
}

mseTest_Dehydration = u[which.min(u)]
bestDehydrationLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_Hyperthermia, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_Hyperthermia, temp))
}

mseTest_Hyperthermia = u[which.min(u)]
bestHyperthermiaLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_AvgSpeed, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_AvgSpeed, temp))
}

mseTest_AvgSpeed = u[which.min(u)]
bestAvgSpeedLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_AvgTravelledDistance, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_AvgTravelledDistance, temp))
}

mseTest_AvgTravelledDistance = u[which.min(u)]
bestAvgTravelledDistanceLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_PressingCapability, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_PressingCapability, temp))
}

mseTest_PressingCapability = u[which.min(u)]
bestPressingCapabilityLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_PhysicalEndurance, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_PhysicalEndurance, temp))
}

mseTest_PhysicalEndurance = u[which.min(u)]
bestPhysicalEnduranceLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_MentalConcentration, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_MentalConcentration, temp))
}

mseTest_MentalConcentration = u[which.min(u)]
bestMentalConcentrationLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_EmotionalMotivation, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_EmotionalMotivation, temp))
}

mseTest_EmotionalMotivation = u[which.min(u)]
bestEmotionalMotivationLambda = lambdaAttempts[which.min(u)]

ridgeMse = c(mseTest_Dehydration, mseTest_Hyperthermia, mseTest_AvgSpeed, mseTest_AvgTravelledDistance, mseTest_PhysicalEndurance, mseTest_MentalConcentration, mseTest_EmotionalMotivation, mseTest_PressingCapability)

# Trying to find out the best Lambda value for a Lasso Model


lambdaAttempts = 10^seq(-3,10,length=50)

Lasso_Dehydration = glmnet(X_train, Y_train$Y_Dehydration, alpha=1, lambda=lambdaAttempts)
Lasso_Hyperthermia = glmnet(X_train, Y_train$Y_Hyperthermia, alpha=1, lambda=lambdaAttempts)
Lasso_AvgSpeed = glmnet(X_train, Y_train$Y_AvgSpeed, alpha=1, lambda=lambdaAttempts)
Lasso_AvgTravelledDistance = glmnet(X_train, Y_train$Y_AvgTravelledDistance, alpha=1, lambda=lambdaAttempts)
Lasso_PressingCapability = glmnet(X_train, Y_train$Y_PressingCapability, alpha=1, lambda=lambdaAttempts)
Lasso_PhysicalEndurance = glmnet(X_train, Y_train$Y_PhysicalEndurance, alpha=1, lambda=lambdaAttempts)
Lasso_MentalConcentration = glmnet(X_train, Y_train$Y_MentalConcentration, alpha=1, lambda=lambdaAttempts)
Lasso_EmotionalMotivation = glmnet(X_train, Y_train$Y_EmotionalMotivation, alpha=1, lambda=lambdaAttempts)

u = c()
temp = c()

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_Dehydration, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_Dehydration, temp))
}

mseTest_Dehydration = u[which.min(u)]
bestDehydrationLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_Hyperthermia, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_Hyperthermia, temp))
}

mseTest_Hyperthermia = u[which.min(u)]
bestHyperthermiaLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_AvgSpeed, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_AvgSpeed, temp))
}

msetest_AvgSpeed = u[which.min(u)]
bestAvgSpeedLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_AvgTravelledDistance, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_AvgTravelledDistance, temp))
}

mseTest_AvgTravelledDistance = u[which.min(u)]
bestAvgTravelledDistanceLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_PressingCapability, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_PressingCapability, temp))
}

mseTest_PressingCapability = u[which.min(u)]
bestPressingCapabilityLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_PhysicalEndurance, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_PhysicalEndurance, temp))
}

mseTest_PhysicalEndurance = u[which.min(u)]
bestPhysicalEnduranceLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_MentalConcentration, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_MentalConcentration, temp))
}

mseTest_MentalConcentration = u[which.min(u)]
bestMentalConcentrationLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_EmotionalMotivation, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_EmotionalMotivation, temp))
}

mseTest_EmotionalMotivation = u[which.min(u)]
bestEmotionalMotivationLambda = lambdaAttempts[which.min(u)]

lassoMse = c(mseTest_Dehydration, mseTest_Hyperthermia, mseTest_AvgSpeed, mseTest_AvgTravelledDistance, mseTest_PhysicalEndurance, mseTest_MentalConcentration, mseTest_EmotionalMotivation, mseTest_PressingCapability)

# PCA 

library(pls)

pca = prcomp(Train[,1:10], scale. = TRUE)
pr_var = pca$sdev^2
propve=pr_var/sum(pr_var)   #proportion of variance explained by each PC
plot(propve, xlab = 'PC', ylab = 'Prop. variance explained', type='b')

pcrDehydration=pcr(Y_Dehydration ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrDehydration, val.type = 'MSEP')
pcrPredDehydration=predict(pcrDehydration, Test, ncomp=8)
mseTest_Dehydration = mse_func(Test$Y_Dehydration, pcrPredDehydration)

pcrHyperthermia=pcr(Y_Hyperthermia ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrHyperthermia, val.type = 'MSEP')
pcrPredHyperthermia=predict(pcrHyperthermia, Test, ncomp=6)
mseTest_Hyperthermia = mse_func(Test$Y_Hyperthermia, pcrPredHyperthermia)

pcrAvgSpeed=pcr(Y_AvgSpeed ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrAvgSpeed, val.type = 'MSEP')
pcrPredAvgSpeed=predict(pcrAvgSpeed, Test, ncomp=10)
mseTest_AvgSpeed = mse_func(Test$Y_AvgSpeed, pcrPredAvgSpeed)

pcrAvgTravelledDistance=pcr(Y_AvgTravelledDistance ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrAvgTravelledDistance, val.type = 'MSEP')
pcrPredAvgTravelledDistance=predict(pcrAvgTravelledDistance, Test, ncomp=10)
mseTest_AvgTravelledDistance= mse_func(Test$Y_AvgTravelledDistance, pcrPredAvgTravelledDistance)

pcrPressingCapability=pcr(Y_PressingCapability ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrPressingCapability, val.type = 'MSEP')
pcrPredPressingCapability=predict(pcrPressingCapability, Test, ncomp=9)
mseTest_PressingCapability = mse_func(Test$Y_PressingCapability, pcrPredPressingCapability)

pcrPhysicalEndurance=pcr(Y_PhysicalEndurance ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrPhysicalEndurance, val.type = 'MSEP')
pcrPredPhysicalEndurance=predict(pcrPhysicalEndurance, Test, ncomp=9)
mseTest_PhysicalEndurance = mse_func(Test$Y_PhysicalEndurance, pcrPredPhysicalEndurance)

pcrMentalConcentration=pcr(Y_MentalConcentration ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrMentalConcentration, val.type = 'MSEP')
pcrPredMentalConcentration=predict(pcrMentalConcentration, Test, ncomp=9)
mseTest_MentalConcentration = mse_func(Test$Y_MentalConcentration, pcrPredMentalConcentration)

pcrEmotionalMotivation=pcr(Y_EmotionalMotivation ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrEmotionalMotivation, val.type = 'MSEP')
pcrPredEmotionalMotivation=predict(pcrEmotionalMotivation, Test, ncomp=7)
mseTest_EmotionalMotivation = mse_func(Test$Y_EmotionalMotivation, pcrPredEmotionalMotivation)

pcrMse = c(mseTest_Dehydration, mseTest_Hyperthermia, mseTest_AvgSpeed, mseTest_AvgTravelledDistance, mseTest_PhysicalEndurance, mseTest_MentalConcentration, mseTest_EmotionalMotivation, mseTest_PressingCapability)

# KNN Regressor

library(caret)

X_train = as.data.frame(scale(X_train))
Y_train = as.data.frame(scale(Y_train))

X_test = as.data.frame(scale(X_test))
Y_test = as.data.frame(scale(Y_test))

# Searching for the best K 

u=c()
temp=c()
for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_Dehydration, k=i) 
  u[i]=c(mse_func(Y_test$Y_Dehydration, predict(temp, X_test)))
}
best_k=which.min(u)
modelDehydration = knnreg(X_train, Y_train$Y_Dehydration, k=best_k)  
PredDehydration = predict(modelDehydration, X_test)   
mseTest_Dehydration=mse_func(Y_test$Y_Dehydration, PredDehydration)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_Hyperthermia, k=i) 
  u[i]=c(mse_func(Y_test$Y_Hyperthermia, predict(temp, X_test)))
}
best_k=which.min(u)
modelHyperthermia = knnreg(X_train, Y_train$Y_Hyperthermia, k=best_k)  
PredHyperthermia = predict(modelHyperthermia, X_test)   
mseTest_Hyperthermia=mse_func(Y_test$Y_Hyperthermia, PredHyperthermia)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_AvgSpeed, k=i) 
  u[i]=c(mse_func(Y_test$Y_AvgSpeed, predict(temp, X_test)))
}
best_k=which.min(u)
modelAvgSpeed = knnreg(X_train, Y_train$Y_AvgSpeed, k=best_k)  
PredAvgSpeed = predict(modelAvgSpeed, X_test)   
mseTest_AvgSpeed=mse_func(Y_test$Y_AvgSpeed, PredAvgSpeed)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_AvgTravelledDistance, k=i) 
  u[i]=c(mse_func(Y_test$Y_AvgTravelledDistance, predict(temp, X_test)))
}
best_k=which.min(u)
modelAvgTravelledDistance = knnreg(X_train, Y_train$Y_AvgTravelledDistance, k=best_k)  
PredAvgTravelledDistance = predict(modelAvgTravelledDistance, X_test)   
mseTest_AvgTravelledDistance=mse_func(Y_test$Y_AvgTravelledDistance, PredAvgTravelledDistance)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_PressingCapability, k=i) 
  u[i]=c(mse_func(Y_test$Y_PressingCapability, predict(temp, X_test)))
}
best_k=which.min(u)
modelPressingCapability = knnreg(X_train, Y_train$Y_PressingCapability, k=best_k)  
PredPressingCapability = predict(modelPressingCapability, X_test)   
mseTest_PressingCapability=mse_func(Y_test$Y_PressingCapability, PredPressingCapability)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_PhysicalEndurance, k=i) 
  u[i]=c(mse_func(Y_test$Y_PhysicalEndurance, predict(temp, X_test)))
}
best_k=which.min(u)
modelPhysicalEndurance = knnreg(X_train, Y_train$Y_PhysicalEndurance, k=best_k)  
PredPhysicalEndurance = predict(modelPhysicalEndurance, X_test)   
mseTest_PhysicalEndurance=mse_func(Y_test$Y_PhysicalEndurance, PredPhysicalEndurance)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_MentalConcentration, k=i) 
  u[i]=c(mse_func(Y_test$Y_MentalConcentration, predict(temp, X_test)))
}
best_k=which.min(u)
modelMentalConcentration = knnreg(X_train, Y_train$Y_MentalConcentration, k=best_k)  
PredMentalConcentration = predict(modelMentalConcentration, X_test)   
mseTest_MentalConcentration=mse_func(Y_test$Y_MentalConcentration, PredMentalConcentration)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_EmotionalMotivation, k=i) 
  u[i]=c(mse_func(Y_test$Y_EmotionalMotivation, predict(temp, X_test)))
}
best_k=which.min(u)
modelEmotionalMotivation = knnreg(X_train, Y_train$Y_EmotionalMotivation, k=best_k)  
PredEmotionalMotivation = predict(modelEmotionalMotivation, X_test)   
mseTest_EmotionalMotivation=mse_func(Y_test$Y_EmotionalMotivation, PredEmotionalMotivation)

knnMse = c(mseTest_Dehydration, mseTest_Hyperthermia, mseTest_AvgSpeed, mseTest_AvgTravelledDistance, mseTest_PhysicalEndurance, mseTest_MentalConcentration, mseTest_EmotionalMotivation, mseTest_PressingCapability)


# PCA with KNN

library(caret)

pca = prcomp(Train[,1:10], scale. = TRUE, retx = TRUE)
X_train = pca$x
Y_train = Train[,11:18]
X_test = Test[,1:10]*pca$rotation
Y_test = Test[,11:18]

u=c()
temp=c()
for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_Dehydration, k=i) 
  u[i]=c(mse_func(Y_test$Y_Dehydration, predict(temp, X_test)))
}
best_k=which.min(u)
modelDehydration = knnreg(X_train, Y_train$Y_Dehydration, k=best_k)  
PredDehydration = predict(modelDehydration, X_test)   
mseTest_Dehydration=mse_func(Y_test$Y_Dehydration, PredDehydration)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_Hyperthermia, k=i) 
  u[i]=c(mse_func(Y_test$Y_Hyperthermia, predict(temp, X_test)))
}
best_k=which.min(u)
modelHyperthermia = knnreg(X_train, Y_train$Y_Hyperthermia, k=best_k)  
PredHyperthermia = predict(modelHyperthermia, X_test)   
mseTest_Hyperthermia=mse_func(Y_test$Y_Hyperthermia, PredHyperthermia)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_AvgSpeed, k=i) 
  u[i]=c(mse_func(Y_test$Y_AvgSpeed, predict(temp, X_test)))
}
best_k=which.min(u)
modelAvgSpeed = knnreg(X_train, Y_train$Y_AvgSpeed, k=best_k)  
PredAvgSpeed = predict(modelAvgSpeed, X_test)   
mseTest_AvgSpeed=mse_func(Y_test$Y_AvgSpeed, PredAvgSpeed)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_AvgTravelledDistance, k=i) 
  u[i]=c(mse_func(Y_test$Y_AvgTravelledDistance, predict(temp, X_test)))
}
best_k=which.min(u)
modelAvgTravelledDistance = knnreg(X_train, Y_train$Y_AvgTravelledDistance, k=best_k)  
PredAvgTravelledDistance = predict(modelAvgTravelledDistance, X_test)   
mseTest_AvgTravelledDistance=mse_func(Y_test$Y_AvgTravelledDistance, PredAvgTravelledDistance)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_PressingCapability, k=i) 
  u[i]=c(mse_func(Y_test$Y_PressingCapability, predict(temp, X_test)))
}
best_k=which.min(u)
modelPressingCapability = knnreg(X_train, Y_train$Y_PressingCapability, k=best_k)  
PredPressingCapability = predict(modelPressingCapability, X_test)   
mseTest_PressingCapability=mse_func(Y_test$Y_PressingCapability, PredPressingCapability)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_PhysicalEndurance, k=i)
  u[i]=c(mse_func(Y_test$Y_PhysicalEndurance, predict(temp, X_test)))
}
best_k=which.min(u)
modelPhysicalEndurance = knnreg(X_train, Y_train$Y_PhysicalEndurance, k=best_k)  
PredPhysicalEndurance = predict(modelPhysicalEndurance, X_test)   
mseTest_PhysicalEndurance=mse_func(Y_test$Y_PhysicalEndurance, PredPhysicalEndurance)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_MentalConcentration, k=i)
  u[i]=c(mse_func(Y_test$Y_MentalConcentration, predict(temp, X_test)))
}
best_k=which.min(u)
modelMentalConcentration = knnreg(X_train, Y_train$Y_MentalConcentration, k=best_k)  
PredMentalConcentration = predict(modelMentalConcentration, X_test)   
mseTest_MentalConcentration=mse_func(Y_test$Y_MentalConcentration, PredMentalConcentration)

for (i in 1:20)
{
  temp = knnreg(X_train, Y_train$Y_EmotionalMotivation, k=i)
  u[i]=c(mse_func(Y_test$Y_EmotionalMotivation, predict(temp, X_test)))
}
best_k=which.min(u)
modelEmotionalMotivation = knnreg(X_train, Y_train$Y_EmotionalMotivation, k=best_k)  
PredEmotionalMotivation = predict(modelEmotionalMotivation, X_test)   
mseTest_EmotionalMotivation=mse_func(Y_test$Y_EmotionalMotivation, PredEmotionalMotivation)

pcaKnnMse = c(mseTest_Dehydration, mseTest_Hyperthermia, mseTest_AvgSpeed, mseTest_AvgTravelledDistance, mseTest_PhysicalEndurance, mseTest_MentalConcentration, mseTest_EmotionalMotivation, mseTest_PressingCapability)

# PLS

X_train = Train[,1:10]
Y_train = Train[,11:18]
X_test = Test[,1:10]
Y_test = Test[,11:18]

library(pls)

plsDehydration=plsr(Y_Dehydration ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(plsDehydration, val.type = 'MSEP')
plsPredDehydration=predict(plsDehydration, Test, ncomp=2)
mseTest_Dehydration = mse_func(Test$Y_Dehydration, plsPredDehydration)

plsHyperthermia=plsr(Y_Hyperthermia ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(plsHyperthermia, val.type = 'MSEP')
plsPredHyperthermia=predict(plsHyperthermia, Test, ncomp=3)
mseTest_Hyperthermia = mse_func(Test$Y_Hyperthermia, plsPredHyperthermia)

plsAvgSpeed=plsr(Y_AvgSpeed ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(plsAvgSpeed, val.type = 'MSEP')
plsPredAvgSpeed=predict(plsAvgSpeed, Test, ncomp=8)
mseTest_AvgSpeed = mse_func(Test$Y_AvgSpeed, plsPredAvgSpeed)

plsAvgTravelledDistance=plsr(Y_AvgTravelledDistance ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(plsAvgTravelledDistance, val.type = 'MSEP')
plsPredAvgTravelledDistance=predict(plsAvgTravelledDistance, Test, ncomp=6)
mseTest_AvgTravelledDistance= mse_func(Test$Y_AvgTravelledDistance, plsPredAvgTravelledDistance)

plsPressingCapability=plsr(Y_PressingCapability ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(plsPressingCapability, val.type = 'MSEP')
plsPredPressingCapability=predict(plsPressingCapability, Test, ncomp=4)
mseTest_PressingCapability = mse_func(Test$Y_PressingCapability, plsPredPressingCapability)

plsPhysicalEndurance=plsr(Y_PhysicalEndurance ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(plsPhysicalEndurance, val.type = 'MSEP')
plsPredPhysicalEndurance=predict(plsPhysicalEndurance, Test, ncomp=3)
mseTest_PhysicalEndurance = mse_func(Test$Y_PhysicalEndurance, plsPredPhysicalEndurance)

plsMentalConcentration=plsr(Y_MentalConcentration ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(plsMentalConcentration, val.type = 'MSEP')
plsPredMentalConcentration=predict(plsMentalConcentration, Test, ncomp=3)
mseTest_MentalConcentration = mse_func(Test$Y_MentalConcentration, plsPredMentalConcentration)

plsEmotionalMotivation=plsr(Y_EmotionalMotivation ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(plsEmotionalMotivation, val.type = 'MSEP')
plsPredEmotionalMotivation=predict(plsEmotionalMotivation, Test, ncomp=2)
mseTest_EmotionalMotivation = mse_func(Test$Y_EmotionalMotivation, plsPredEmotionalMotivation)

plsMse = c(mseTest_Dehydration, mseTest_Hyperthermia, mseTest_AvgSpeed, mseTest_AvgTravelledDistance, mseTest_PhysicalEndurance, mseTest_MentalConcentration, mseTest_EmotionalMotivation, mseTest_PressingCapability)

#Euristic Method for Best Subset Selection (Backward)

library(leaps)
library(tidyverse)
library(caret)

train.control = trainControl(method = "cv", number = 10)
step.model1 = train( Y_Dehydration~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model1$results

step.model2 = train( Y_Hyperthermia~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model2$results

step.model3 = train( Y_AvgSpeed~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model3$results


step.model4 = train( Y_AvgTravelledDistance~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model4$results


step.model5 = train( Y_PressingCapability~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model5$results


step.model6 = train( Y_PhysicalEndurance~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model6$results


step.model7 = train( Y_MentalConcentration~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model7$results


step.model8 = train( Y_EmotionalMotivation~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model8$results

#Euristic Method for Best Subset Selection (Backward)

step.model1 = train( Y_Dehydration~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model1$results


step.model2 = train( Y_Hyperthermia~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model2$results


step.model3 = train( Y_AvgSpeed~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model3$results


step.model4 = train( Y_AvgTravelledDistance~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model4$results


step.model5 = train( Y_PressingCapability~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model5$results


step.model6 = train( Y_PhysicalEndurance~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model6$results


step.model7 = train( Y_MentalConcentration~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model7$results


step.model8 = train( Y_EmotionalMotivation~X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data = Train,
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:10),
                      trControl = train.control
)
step.model8$results

# Plotting the methods' comparison

library(plot3D)

z = cbind(knnMse, plsMse, mlrMse, ridgeMse, lassoMse, pcrMse, pcaKnnMse)
hist3D (z = z, colvar = z, col = NULL,  add = FALSE, space = 0.5, theta=-40, phi=50, xlab='Variables', ylab='Methods', zlab='MSE')
