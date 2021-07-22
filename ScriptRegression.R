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

mseTrain_Dehydration = mse_func(Train$Y_Dehydration, predict(RegDehydration, Train))
mseTest_Dehydration = mse_func(Test$Y_Dehydration, predict(RegDehydration, Test))

mseTrain_Hyperthermia = mse_func(Train$Y_Hyperthermia, predict(RegHyperthermia, Train))
mseTest_Hyperthermia = mse_func(Test$Y_Hyperthermia, predict(RegHyperthermia, Test))

mseTrain_AvgSpeed = mse_func(Train$Y_AvgSpeed, predict(RegAvgSpeed, Train))
mseTest_AvgSpeed = mse_func(Test$Y_AvgSpeed, predict(RegAvgSpeed, Test))

mseTrain_AvgTravelledDistance = mse_func(Train$Y_AvgTravelledDistance, predict(RegAvgTravelledDistance, Train))
mseTest_AvgTravelledDistance = mse_func(Test$Y_AvgTravelledDistance, predict(RegAvgTravelledDistance, Test))

mseTrain_PressingCapability = mse_func(Train$Y_PressingCapability, predict(RegPressingCapability, Train))
mseTest_PressingCapability = mse_func(Test$Y_PressingCapability, predict(RegPressingCapability, Test))

mseTrain_PhysicalEndurance = mse_func(Train$Y_PhysicalEndurance, predict(RegPhysicalEndurance, Train))
mseTest_PhysicalEndurance = mse_func(Test$Y_PhysicalEndurance, predict(RegPhysicalEndurance, Test))

mseTrain_MentalConcentration = mse_func(Train$Y_MentalConcentration, predict(RegMentalConcentration, Train))
mseTest_MentalConcentration = mse_func(Test$Y_MentalConcentration, predict(RegMentalConcentration, Test))

mseTrain_EmotionalMotivation = mse_func(Train$Y_EmotionalMotivation, predict(RegEmotionalMotivation, Train))
mseTest_EmotionalMotivation = mse_func(Test$Y_EmotionalMotivation, predict(RegEmotionalMotivation, Test))

# Ridge and Lasso subset selection

X_train = Train[,1:10]
Y_train = Train[,11:18]
X_test = Test[,1:10]
Y_test = Test[,11:18]
my_lambda = 0.1

Ridge_Dehydration = glmnet(X_train, Y_train$Y_Dehydration, alpha=0, lambda=my_lambda)
Ridge_Hyperthermia = glmnet(X_train, Y_train$Y_Hyperthermia, alpha=0, lambda=my_lambda)
Ridge_AvgSpeed = glmnet(X_train, Y_train$Y_AvgSpeed, alpha=0, lambda=my_lambda)
Ridge_AvgTravelledDistance = glmnet(X_train, Y_train$Y_AvgTravelledDistance, alpha=0, lambda=my_lambda)
Ridge_PressingCapability = glmnet(X_train, Y_train$Y_PressingCapability, alpha=0, lambda=my_lambda)
Ridge_PhysicalEndurance = glmnet(X_train, Y_train$Y_PhysicalEndurance, alpha=0, lambda=my_lambda)
Ridge_MentalConcentration = glmnet(X_train, Y_train$Y_MentalConcentration, alpha=0, lambda=my_lambda)
Ridge_EmotionalMotivation = glmnet(X_train, Y_train$Y_EmotionalMotivation, alpha=0, lambda=my_lambda)

Lasso_Dehydration = glmnet(X_train, Y_train$Y_Dehydration, alpha=1, lambda=0.001)
Lasso_Hyperthermia = glmnet(X_train, Y_train$Y_Hyperthermia, alpha=1, lambda=my_lambda)
Lasso_AvgSpeed = glmnet(X_train, Y_train$Y_AvgSpeed, alpha=1, lambda=my_lambda)
Lasso_AvgTravelledDistance = glmnet(X_train, Y_train$Y_AvgTravelledDistance, alpha=1, lambda=my_lambda)
Lasso_PressingCapability = glmnet(X_train, Y_train$Y_PressingCapability, alpha=1, lambda=my_lambda)
Lasso_PhysicalEndurance = glmnet(X_train, Y_train$Y_PhysicalEndurance, alpha=1, lambda=my_lambda)
Lasso_MentalConcentration = glmnet(X_train, Y_train$Y_MentalConcentration, alpha=1, lambda=my_lambda)
Lasso_EmotionalMotivation = glmnet(X_train, Y_train$Y_EmotionalMotivation, alpha=1, lambda=my_lambda)

# Ridge MSE

mseTrain_Dehydration = mse_func(Train$Y_Dehydration, predict(Ridge_Dehydration, s = my_lambda, newx=as.matrix(X_train)))
mseTest_Dehydration = mse_func(Test$Y_Dehydration, predict(Ridge_Dehydration, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_Hyperthermia = mse_func(Train$Y_Hyperthermia, predict(Ridge_Hyperthermia, s = my_lambda, newx=as.matrix(X_train)))
mseTest_Hyperthermia = mse_func(Test$Y_Hyperthermia, predict(Ridge_Hyperthermia, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_AvgSpeed = mse_func(Train$Y_AvgSpeed, predict(Ridge_AvgSpeed, s = my_lambda, newx=as.matrix(X_train)))
mseTest_AvgSpeed = mse_func(Test$Y_AvgSpeed, predict(Ridge_AvgSpeed, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_AvgTravelledDistance = mse_func(Train$Y_AvgTravelledDistance, predict(Ridge_AvgTravelledDistance, s = my_lambda, newx=as.matrix(X_train)))
mseTest_AvgTravelledDistance = mse_func(Test$Y_AvgTravelledDistance, predict(Ridge_AvgTravelledDistance, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_PressingCapability = mse_func(Train$Y_PressingCapability, predict(Ridge_PressingCapability, s = my_lambda, newx=as.matrix(X_train)))
mseTest_PressingCapability = mse_func(Test$Y_PressingCapability, predict(Ridge_PressingCapability, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_PhysicalEndurance = mse_func(Train$Y_PhysicalEndurance, predict(Ridge_PhysicalEndurance, s = my_lambda, newx=as.matrix(X_train)))
mseTest_PhysicalEndurance = mse_func(Test$Y_PhysicalEndurance, predict(Ridge_PhysicalEndurance, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_MentalConcentration = mse_func(Train$Y_MentalConcentration, predict(Ridge_MentalConcentration, s = my_lambda, newx=as.matrix(X_train)))
mseTest_MentalConcentration = mse_func(Test$Y_MentalConcentration, predict(Ridge_MentalConcentration, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_EmotionalMotivation = mse_func(Train$Y_EmotionalMotivation, predict(Ridge_EmotionalMotivation, s = my_lambda, newx=as.matrix(X_train)))
mseTest_EmotionalMotivation = mse_func(Test$Y_EmotionalMotivation, predict(Ridge_EmotionalMotivation, s = my_lambda, newx=as.matrix(X_test)))


# Lasso MSE


mseTrain_Dehydration = mse_func(Train$Y_Dehydration, predict(Lasso_Dehydration, s = my_lambda, newx=as.matrix(X_train)))
mseTest_Dehydration = mse_func(Test$Y_Dehydration, predict(Lasso_Dehydration, s = 0.001, newx=as.matrix(X_test)))

mseTrain_Hyperthermia = mse_func(Train$Y_Hyperthermia, predict(Lasso_Hyperthermia, s = my_lambda, newx=as.matrix(X_train)))
mseTest_Hyperthermia = mse_func(Test$Y_Hyperthermia, predict(Lasso_Hyperthermia, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_AvgSpeed = mse_func(Train$Y_AvgSpeed, predict(Lasso_AvgSpeed, s = my_lambda, newx=as.matrix(X_train)))
mseTest_AvgSpeed = mse_func(Test$Y_AvgSpeed, predict(Lasso_AvgSpeed, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_AvgTravelledDistance = mse_func(Train$Y_AvgTravelledDistance, predict(Lasso_AvgTravelledDistance, s = my_lambda, newx=as.matrix(X_train)))
mseTest_AvgTravelledDistance = mse_func(Test$Y_AvgTravelledDistance, predict(Lasso_AvgTravelledDistance, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_PressingCapability = mse_func(Train$Y_PressingCapability, predict(Lasso_PressingCapability, s = my_lambda, newx=as.matrix(X_train)))
mseTest_PressingCapability = mse_func(Test$Y_PressingCapability, predict(Lasso_PressingCapability, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_PhysicalEndurance = mse_func(Train$Y_PhysicalEndurance, predict(Lasso_PhysicalEndurance, s = my_lambda, newx=as.matrix(X_train)))
mseTest_PhysicalEndurance = mse_func(Test$Y_PhysicalEndurance, predict(Lasso_PhysicalEndurance, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_MentalConcentration = mse_func(Train$Y_MentalConcentration, predict(Lasso_MentalConcentration, s = my_lambda, newx=as.matrix(X_train)))
mseTest_MentalConcentration = mse_func(Test$Y_MentalConcentration, predict(Lasso_MentalConcentration, s = my_lambda, newx=as.matrix(X_test)))

mseTrain_EmotionalMotivation = mse_func(Train$Y_EmotionalMotivation, predict(Lasso_EmotionalMotivation, s = my_lambda, newx=as.matrix(X_train)))
mseTest_EmotionalMotivation = mse_func(Test$Y_EmotionalMotivation, predict(Lasso_EmotionalMotivation, s = my_lambda, newx=as.matrix(X_test)))


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

bestDehydrationLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_Hyperthermia, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_Hyperthermia, temp))
}

bestHyperthermiaLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_AvgSpeed, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_AvgSpeed, temp))
}

bestAvgSpeedLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_AvgTravelledDistance, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_AvgTravelledDistance, temp))
}

bestAvgTravelledDistanceLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_PressingCapability, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_PressingCapability, temp))
}

bestPressingCapabilityLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_PhysicalEndurance, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_PhysicalEndurance, temp))
}

bestPhysicalEnduranceLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_MentalConcentration, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_MentalConcentration, temp))
}

bestMentalConcentrationLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Ridge_EmotionalMotivation, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_EmotionalMotivation, temp))
}

bestEmotionalMotivationLambda = lambdaAttempts[which.min(u)]

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

bestDehydrationLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_Hyperthermia, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_Hyperthermia, temp))
}

bestHyperthermiaLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_AvgSpeed, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_AvgSpeed, temp))
}

bestAvgSpeedLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_AvgTravelledDistance, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_AvgTravelledDistance, temp))
}

bestAvgTravelledDistanceLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_PressingCapability, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_PressingCapability, temp))
}

bestPressingCapabilityLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_PhysicalEndurance, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_PhysicalEndurance, temp))
}

bestPhysicalEnduranceLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_MentalConcentration, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_MentalConcentration, temp))
}

bestMentalConcentrationLambda = lambdaAttempts[which.min(u)]

for (i in 1:length(lambdaAttempts))
{
  temp=predict(Lasso_EmotionalMotivation, s=lambdaAttempts[i], newx = as.matrix(X_test))
  u[i]=c(mse_func(Y_test$Y_EmotionalMotivation, temp))
}

bestEmotionalMotivationLambda = lambdaAttempts[which.min(u)]

# PCA 

pca = prcomp(Train[,1:10], scale. = TRUE)
pr_var = pca$sdev^2
propve=pr_var/sum(pr_var)   #proportion of variance explained by each PC
plot(propve, xlab = 'PC', ylab = 'Prop. variance explained', type='b')

pcrDehydration=pcr(Y_Dehydration ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrDehydration, val.type = 'MSEP')
pcrPredDehydration=predict(pcrDehydration, Test, ncomp=6)
pcrMseDehydration = mse_func(Test$Y_Dehydration, pcrPredDehydration)

pcrHyperthermia=pcr(Y_Hyperthermia ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrHyperthermia, val.type = 'MSEP')
pcrPredHyperthermia=predict(pcrHyperthermia, Test, ncomp=6)
pcrMseHyperthermia = mse_func(Test$Y_Hyperthermia, pcrPredHyperthermia)

pcrAvgSpeed=pcr(Y_AvgSpeed ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrAvgSpeed, val.type = 'MSEP')
pcrPredAvgSpeed=predict(pcrAvgSpeed, Test, ncomp=6)
pcrMseAvgSpeed = mse_func(Test$Y_AvgSpeed, pcrPredAvgSpeed)

pcrAvgTravelledDistance=pcr(Y_AvgTravelledDistance ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrAvgTravelledDistance, val.type = 'MSEP')
pcrPredAvgTravelledDistance=predict(pcrAvgTravelledDistance, Test, ncomp=6)
pcrMseAvgTravelledDistance= mse_func(Test$Y_AvgTravelledDistance, pcrPredAvgTravelledDistance)

pcrPressingCapability=pcr(Y_PressingCapability ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrPressingCapability, val.type = 'MSEP')
pcrPredPressingCapability=predict(pcrPressingCapability, Test, ncomp=6)
pcrMsePressingCapability = mse_func(Test$Y_PressingCapability, pcrPredPressingCapability)

pcrPhysicalEndurance=pcr(Y_PhysicalEndurance ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrPhysicalEndurance, val.type = 'MSEP')
pcrPredPhysicalEndurance=predict(pcrPhysicalEndurance, Test, ncomp=6)
pcrMsePhysicalEndurance = mse_func(Test$Y_PhysicalEndurance, pcrPredPhysicalEndurance)

pcrMentalConcentration=pcr(Y_MentalConcentration ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrMentalConcentration, val.type = 'MSEP')
pcrPredMentalConcentration=predict(pcrMentalConcentration, Test, ncomp=6)
pcrMseMentalConcentration = mse_func(Test$Y_MentalConcentration, pcrPredMentalConcentration)

pcrEmotionalMotivation=pcr(Y_EmotionalMotivation ~ X_Temperature + X_Humidity + X_Altitude + X_ClimaticConditions + X_RestTimeFromLastMatch + X_AvgPlayerValue + X_MatchRelevance + X_AvgGoalConcededLastMatches + X_SupportersImpact + X_OpposingSupportersImpact, data=Train, scale=TRUE, validation='CV')
validationplot(pcrEmotionalMotivation, val.type = 'MSEP')
pcrPredEmotionalMotivation=predict(pcrEmotionalMotivation, Test, ncomp=6)
pcrMseEmotionalMotivation = mse_func(Test$Y_EmotionalMotivation, pcrPredEmotionalMotivation)

