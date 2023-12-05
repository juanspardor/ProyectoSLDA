#Intentos reales

set.seed(777)
#Vamos a sacar una submuestra para hacer pruebas
nTrain = nrow(trainOut)
indicesTrain = sample(c(1:nTrain), 76000, replace = FALSE)

#Train subset 
trainModelos = trainOut[indicesTrain,]
testModelos = trainOut[-indicesTrain,]
yTestModelos = testModelos$V1

#1. Random forest
library(randomForest)

mtrs = c(3,6,9,12,15)
nodos = c(2,3,4,5,6)
matrizRF = matrix(0,5,5)

for(i in 1:5){
  for(j in 1:5){
    mact = mtrs[i]
    mxact = nodos[j]
    print(paste(mact,mxact,sep = "-"))
    rfAct = randomForest(V1~., data = trainModelos, mtry=mact, maxnodes=mxact, ntree = 500)
    pRF = predict(rfAct, testModelos)
    matrizRF[i,j] = (1/76000)*sum((yTestModelos-pRF)^2)
  }
}

matrizRF = matrizRF*76000/1609
min(matrizRF)
#El que da mejor es la ultima

modeloRFTuned = randomForest(V1~., data = trainOut, mtry = 15, maxnodes = 6, ntree = 500)
predRFTn = predict(modeloRFTuned, testReg)
result_df <- data.frame(Id = 1:length(predRFTn), y = predRFTn)
camino = "/Users/juanse/Downloads/resultadosRFtuned2.csv"
write.csv(result_df, file = camino, row.names = FALSE)


#2. Bagging



#3. Boosting
library(gbm)

depts = c(3,6,9,12,15)
shrinks = seq(from = 0, to = 1, by = 0.2)
matrizBoost = matrix(0, 5, 6)

for(i in 1:5)
{
  for(j in 1:6)
  {
    depAct = depts[i]
    sAct = shrinks[j]
    print(paste(depAct,sAct,sep = "-"))
    bAct = gbm(V1~.,data=trainOut,
               n.trees=100,interaction.depth=depAct,shrinkage=sAct)
    
    pB = predict(bAct, testModelos)
    matrizBoost[i,j] = (1/1609)*sum((yTestModelos-pB)^2)
  }
}
matrizBoost
min(matrizBoost)

boost=gbm(V1~.,data=trainOut,
              n.trees=100,interaction.depth=15,shrinkage=0.4)

pBoost = predict(boost, testReg)
result_df <- data.frame(Id = 1:length(pBoost), y = pBoost)
camino = "/Users/juanse/Downloads/resultadosBoostTuned.csv"
write.csv(result_df, file = camino, row.names = FALSE)

summary(boost.car)

#4. GAM
library(gam)
gams.fit <- gam(V1~s(V2)+s(V3)+s(V5)+s(V6)+s(V7)+s(V8)+s(V9)+s(V10)+
                  s(V11)+s(V12)+s(V13)+s(V14)+s(V15)+s(V16)+s(V17)+s(V18)+s(V19)
                +s(V20)+s(V21)+s(V22)+s(V23)+s(V24)+s(V25)+s(V26)+s(V27)+s(V28)+s(V29)
                +s(V30)+s(V31)+s(V32)+s(V33)+s(V34)+s(V35)+s(V36)+s(V37)+s(V38)+s(V39)
                +s(V40)+s(V41)+s(V42)+s(V43)+s(V44)+s(V45)+s(V46)+s(V47)+s(V48)+s(V49)
                +s(V50)+s(V51)+s(V52)+s(V53)+s(V54)+s(V55)+s(V56)+s(V57)+s(V58)+s(V59)
                +s(V60)+s(V61)+s(V62)+s(V63)+s(V64)+s(V65)+s(V66)+s(V67)+s(V68)+s(V69)
                +s(V70)+s(V71)+s(V72)+s(V73)+s(V74)+s(V75)+s(V76)+s(V77)+s(V78)+s(V79)
                +s(V80)+s(V81)+s(V82)+s(V83)+s(V84)+s(V85)+s(V86)+s(V87)+s(V88)+s(V89)+s(V90)+s(V91),data=trainOut)

summary(gams.fit)
predGamOut = predict(gams.fit, testReg)
result_df <- data.frame(Id = 1:length(predGamOut), y = predGamOut)
camino = "/Users/juanse/Downloads/resultadosFinal.csv"
write.csv(result_df, file = camino, row.names = FALSE)
