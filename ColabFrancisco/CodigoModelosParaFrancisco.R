dataset="UWaveGestureLibrary"
directorioFicheros="/home/fjbaldan/01_Drive/0_Trabajo/0_Investigacion/00_MultiVariableTimeSeries/AragonColab_FeatureSelection/"
train= read.csv(file=paste0(directorioFicheros,dataset,"_TRAIN.csv"))
test= read.csv(file=paste0(directorioFicheros,dataset,"_TEST.csv"))

train[,2]=as.factor(train[,2])
test[,2]=as.factor(test[,2])

library(C50)
library(randomForest)
# Codigo de ejecución de modelos, con una lectura de los csv se pueden aplicar directamente.
model=C5.0(x=train[,-c(1,2)],y = train[,2],trials = 10)
pred=predict(model,newdata=test[,-c(1,2)])
accC5=mean(pred==test[,2])
accC5
print("accC5")
print(accC5)

# options(expressions = 500000)
model=randomForest(tsClass ~ .,train[,-1])
# model=randomForest(tsClass ~ .,data.matrix(train[,-1]))
pred=predict(model,newdata=test[,-c(1,2)])
accRF=mean(pred==test[,2])
accRF
print("accRF")
print(accRF)
