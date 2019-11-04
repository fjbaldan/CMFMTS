# Mi modelo optimizado
# R --max-ppsize=500000
library(cmfts)
library(data.table)
library(gtools)
library(parallel)
csvdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_csv/"
outputdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_CMFTS/"
outputdir2="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_CMFTS/AragonColab_FeatureSelection/"
datasets=as.character(unlist(read.table("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/Selected_UEA_Datasets.txt", sep = ",")))

results=mclapply(datasets,function(dataset){
  set.seed(26)
  print(dataset)

  train=fread(file = paste0(outputdir,dataset,"_TRAIN.csv"),data.table=FALSE)
  test=fread(file = paste0(outputdir,dataset,"_TEST.csv"),data.table=FALSE)

  all=rbind(train,test)
  all[,2]=as.factor(all[,2])
  train=all[1:dim(train)[1],]
  test=all[(dim(train)[1]+1):dim(all)[1],]

  # Todos numéricos, incluso los NA
  train[,-c(1,2)]=apply(train[,-c(1,2)],c(1,2),as.numeric)
  test[,-c(1,2)]=apply(test[,-c(1,2)],c(1,2),as.numeric)

  # Eliminar variables full Na o NaN
  train=train[colSums(!is.na(train)) > 0]
  train=train[colSums(apply(train,c(1,2),function(x){
    !is.nan(x)
  }))>0]
  test=test[colSums(!is.na(test)) > 0]
  test=test[colSums(apply(test,c(1,2),function(x){
    !is.nan(x)
  }))>0]

  # Imputamos con la media los valores perdidos
  # Comprobado
  train[,-c(1,2)]=apply(train[,-c(1,2)],2,function(x){
    na.replace(x, mean(x,na.rm = TRUE))
  })

  test[,-c(1,2)]=apply(test[,-c(1,2)],2,function(x){
    na.replace(x, mean(x,na.rm = TRUE))
  })

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

  # Exportamos ficheros para Francisco
  write.table(file = paste0(outputdir2,dataset,"_TRAIN.csv"),train,row.names = F,sep = ",")
  write.table(file = paste0(outputdir2,dataset,"_TEST.csv"),test,row.names = F,sep=",")

  # Exportamos ficheros para Francisco
  c(dataset,  length(unique(train[,2])), length(train[,1]), length(train[1,-c(1,2)]))

},mc.cores = 4)

out=as.data.frame(do.call(rbind,results))
names(out)=c("dataset","num_classes","num_instances","num_variables")

write.csv(out,file = paste0(outputdir2,"DataDatasetsFrancisco.csv"),row.names = F)

