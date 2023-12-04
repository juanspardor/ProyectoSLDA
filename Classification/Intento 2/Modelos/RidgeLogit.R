library(randomForest)
library(readr)
dataset <- read_csv("2023-2/statistical/Proyecto/ProyectoSLDA/Classification/Intento 1/Modelos/train.csv")
head(dataset)


rf=randomForest(FRACASO~.-CODIGO_EMPRESA,dataset)
varImpPlot(rf)

imp=importance(rf)
sort(imp)
imp
plot(imp)

X=dataset[,3:ncol(dataset)];head(X)

pp=princomp(X,scores=TRUE,cor=TRUE)
summary(pp,loadings=TRUE)

plot(pp$scores)
pp$scale
biplot(pp)
varBasuras=c('B11','B13','B14','B21','B23','B24','B46','B48',
            'B51','B52','B53','B54','B61','B62','B63','B64',
            'B65','B66','B67','B611','B85')
XX=dataset[,-which(names(dataset)%in%varBasuras)];head(XX)
XX=XX[,3:ncol(XX)];head(XX)

pp2<-princomp(XX,scores=TRUE,cor=TRUE)
plot(pp2$scores,col=dataset$FRACASO,lwd=3)
summary(pp2)
plot(pp$sdev^2,type="b",main="Varianzas de Componentes")
pp$sdev

colnames(XX)

pr<-prcomp(XX,)


variables<-c('B47','B45','B46','B22','B24','B21','B31','B23','B67',
             'B83','B42','B32','B48','B63','B43','B610','B612',
             'B44','B52','B13','B68','B61','B85','B82','B611')
X3<-dataset[,which(names(dataset)%in%variables)];head(X3)
X4<-X3[,1:ncol(X3)]
head(X4)


corr=cor(X3)
heatmap(corr, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Heatmap of Correlation Matrix",
        cex.col = 1.5, cex.row = 1.5)

scaled_X3<-scale(X3)
centering_factors <- attr(scaled_X3, "scaled:center")
scaling_factors <- attr(scaled_X3, "scaled:scale")


test <- read_csv("2023-2/statistical/Proyecto/ProyectoSLDA/Classification/Intento 1/Modelos/test.csv")
TEST<-test[,which(names(test)%in%variables)]
scaled_TEST <- scale(TEST, center = centering_factors, scale = scaling_factors)

X3$y=as.factor(dataset$FRACASO)


lambda_values
library(glmnet)
X3<-as.matrix(X3)
lasso<-cv.glmnet(X3,dataset$FRACASO,alpha=0,family='binomial',nfolds=10)

lambda=lasso$lambda.min
lambda

predictions=predict(lasso,scaled_TEST,type='response')


# Create a data frame with "Id" and "Probabilities" columns
result_df <- data.frame(Id = seq_along(predictions), Probabilities = predictions)

# Write the data frame to a CSV file
write.csv(result_df, file = "predictions.csv", row.names = FALSE)


install.packages(DMwR)
library(DMwR2)


variables2<-c('B47','B45','B46','B22','B24','B21','B31','B23','B67',
             'B83','B42','B32','B48','B63','B43','B610','B612',
             'B44','B52','B13','B68','B61','B85','B82','B611','FRACASO')

aa<-dataset[]

outlier.scores<-lofactor(X3,k=4)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:10]
print(outliers)
row.names(X3)[outliers]



install.packages(mvoutlier)
library(mvoutlier)
cars=mtcars[,c("mpg","disp","hp","drat")]
head(cars)
chisq.plot(X3)
uni.plot(cars)

