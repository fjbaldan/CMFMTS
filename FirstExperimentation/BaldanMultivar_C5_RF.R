# Mi modelo optimizado
# R --max-ppsize=500000
library(ccmfbd)
library(data.table)
library(gtools)
library(parallel)
library(C50)
library(randomForest)
library(caret)
csvdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_csv/"
outputdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_CMFTS/"
outputdir2="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_CMFTS/AragonFeatureSelection/"
datasets=as.character(unlist(read.table("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/Selected_UEA_Datasets.txt", sep = ",")))

results=mclapply(datasets,function(dataset){
# results=lapply(datasets,function(dataset){
  set.seed(26)
  print(dataset)

  train=fread(file = paste0(outputdir,dataset,"_TRAIN.csv"),data.table=FALSE)
  test=fread(file = paste0(outputdir,dataset,"_TEST.csv"),data.table=FALSE)

  print("1")

  all=rbind(train,test)
  all[,2]=as.factor(all[,2])
  train=all[1:dim(train)[1],]
  test=all[(dim(train)[1]+1):dim(all)[1],]

  # Todos numéricos, incluso los NA
  train[,-c(1,2)]=apply(train[,-c(1,2)],c(1,2),as.numeric)
  test[,-c(1,2)]=apply(test[,-c(1,2)],c(1,2),as.numeric)

  print("2")

  # Eliminar variables full Na o NaN
  train=train[colSums(!is.na(train)) > 0]
  train=train[colSums(apply(train,c(1,2),function(x){
    !is.nan(x)
  }))>0]
  test=test[colSums(!is.na(test)) > 0]
  test=test[colSums(apply(test,c(1,2),function(x){
    !is.nan(x)
  }))>0]

  print("3")

  # Imputamos con la media los valores perdidos
  # Comprobado
  train[,-c(1,2)]=apply(train[,-c(1,2)],2,function(x){
    na.replace(x, mean(x,na.rm = TRUE))
  })

  test[,-c(1,2)]=apply(test[,-c(1,2)],2,function(x){
    na.replace(x, mean(x,na.rm = TRUE))
  })

  print("4")

  # El dataset de test ha de tener las mismas columnas que el dataset de entrada,
  # si la variable se ha eliminado por todo Na NaN se rellena a 0
  newTest=data.frame(matrix(0, nrow=dim(test)[1], ncol=dim(train)[2]))
  names(newTest)=names(train)
  sel=names(train)[names(train)%in%names(test)]
  for(col in sel){
    newTest[,col]=test[,col]
  }
  test=newTest

  # Seleccion de características:

  # Quitamos columnas sin informacion (Es necesario para la autocorrelacion)
  colQuitar=which(apply(train,2,function(x){
    length(unique(x))<=1
  }))

  train=train[,-colQuitar]
  test=test[,-colQuitar]

  # Exportamos fichero para Francisco
  train=write.table(file = paste0(outputdir2,dataset,"_TRAIN.csv"),train,row.names = F,sep = ",")
  test=write.table(file = paste0(outputdir2,dataset,"_TEST.csv"),test,row.names = F,sep=",")


  # Correladas
  correlationMatrix <- cor(train[,-c(1,2)])
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.9)

  train=cbind(train[,c(1,2)],train[,-c(1,2)][,-highlyCorrelated])
  test=cbind(test[,c(1,2)],test[,-c(1,2)][,-highlyCorrelated])


  # # Recursive feature elimination
  # control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  # # run the RFE algorithm
  # results <- rfe(train[,-c(1,2)], train[,2], sizes=1:(length(train[1,-c(1,2)])), rfeControl=control)
  #
  # var_index=sapply(predictors(results), function(x){
  #   which(x==names(train))
  # })
  #
  # # Seleccionamos el 50% mas importante, si obtenemos una seleccion inferior al 50% del total, las tomamos todas.
  # if(length(predictors(results))<(round(length(train[1,-c(1,2)])*50/100))){
  #   var_index=var_index
  # }else{
  #   var_index=var_index[1:(round(length(var_index)*50/100))]
  # }
  #
  # train=cbind(train[,c(1,2)],train[,var_index])
  # test=cbind(test[,c(1,2)],test[,var_index])

  print("models")

  output = tryCatch({
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

    c(accC5,accRF)
  }, warning = function(w) {
    print(w)
    c(NaN,NaN)
  }, error = function(e) {
    print(e)
    c(NA,NA)
  }, finally = {

  }
  )
  print(c(dataset,output))
  c(dataset,output)
  },mc.cores = 30)
# })

out=as.data.frame(do.call(rbind,results))
names(out)=c("dataset","cmfts_C5_acc","cmfts_RF_acc")

# Con 2000 árboles no mejora:
# Probar: un modelo general (todas las variables en el mismo dataset) que clasifique cada variable y se elija por mayoria
# si hay un empate se elige de forma aleatoria.
# Probar: Un dataset por cada variable y un modelo por cada dataset. El resultado final se elige de igual forma que en el caso anterior.

write.table(out,file="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/CMFTS_Optimizado_Corr09_results.csv",sep=",",row.names = F)
# write.table(out,file="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/CMFTS_Optimizado_RFE50_results.csv",sep=",",row.names = F)
# write.table(out,file="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/CMFTS_Optimizado_RFE_results.csv",sep=",",row.names = F)
