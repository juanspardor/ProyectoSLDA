#Prueba porque si

library(randomForest)
randFor = randomForest(V1~., data = trainReg, mtry=9, maxnodes=3, ntree = 1000)
predicciones = predict(randFor, testReg)

result_df <- data.frame(Id = 1:length(predicciones), y = predicciones2)

camino = "/Users/juanse/Downloads/resultados.csv"
write.csv(result_df, file = camino, row.names = FALSE)
randFor$ntree


predicciones2 = predict(randFor, testReg)
camino = "/Users/juanse/Downloads/resultados2.csv"
write.csv(result_df, file = camino, row.names = FALSE)


library(gam)
gams.fit <- gam(V1~s(V2)+s(V3)+s(V5)+s(V6)+s(V7)+s(V8)+s(V9)+s(V10)+
                  s(V11)+s(V12)+s(V13)+s(V14)+s(V15)+s(V16)+s(V17)+s(V18)+s(V19)
                +s(V20)+s(V21)+s(V22)+s(V23)+s(V24)+s(V25)+s(V26)+s(V27)+s(V28)+s(V29)
                +s(V30)+s(V31)+s(V32)+s(V33)+s(V34)+s(V35)+s(V36)+s(V37)+s(V38)+s(V39)
                +s(V40)+s(V41)+s(V42)+s(V43)+s(V44)+s(V45)+s(V46)+s(V47)+s(V48)+s(V49)
                +s(V50)+s(V51)+s(V52)+s(V53)+s(V54)+s(V55)+s(V56)+s(V57)+s(V58)+s(V59)
                +s(V60)+s(V61)+s(V62)+s(V63)+s(V64)+s(V65)+s(V66)+s(V67)+s(V68)+s(V69)
                +s(V70)+s(V71)+s(V72)+s(V73)+s(V74)+s(V75)+s(V76)+s(V77)+s(V78)+s(V79)
                +s(V80)+s(V81)+s(V82)+s(V83)+s(V84)+s(V85)+s(V86)+s(V87)+s(V88)+s(V89)+s(V90)+s(V91),data=trainReg)

predicciones3 = predict(gams.fit, testReg)
result_df <- data.frame(Id = 1:length(predicciones), y = predicciones3)
camino = "/Users/juanse/Downloads/resultados3.csv"
write.csv(result_df, file = camino, row.names = FALSE)


#Hagamos la prueba con Boosting
library(gbm)

boost = gbm(V1~., data = trainReg, n.trees = 200)
predicciones5 = predict(boost, testReg)
result_df <- data.frame(Id = 1:length(predicciones4), y = predicciones5)
camino = "/Users/juanse/Downloads/resultados5.csv"
write.csv(result_df, file = camino, row.names = FALSE)
