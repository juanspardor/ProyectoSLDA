library(Boruta)
library(randomForest)
library(readr)
datos <- read_csv("2023-2/statistical/Proyecto/ProyectoSLDA/Classification/TrainCsv.csv")

X=datos[,-(1:2)];head(X)
y=datos$FRACASO
y<-as.factor(y)
length(y)
clf<-table(y);clf
cw<-1/prop.table(clf);cw

rf<-randomForest(X,y,class.weights=cw)
varImpPlot(rf)

boruta<-Boruta(X,y)
boruta
install.packages('boruta')
library(boruta)

install.packages("DMwR")
install.packages("ROSE")
library(ROSE)
rose<-ROSE(FRACASO~.,data=datos,seed=123)
rose
Xnew=rose$data[,-(1:2)];head(Xnew)
yNew=rose$data$FRACASO

Boruta(Xnew,yNew)
