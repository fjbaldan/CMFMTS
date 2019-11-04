library(foreign)
workspace="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_arff/"

# # Listado creado de datasets
# workspace="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_arff/"
# datasets=basename(list.dirs(workspace,full.names = F))[-1][-c(6,11,16)]
# write.table(datasets,file="ALL_UEA_Datasets.txt",sep=",", col.names = F, row.names = F)

# datos=read.table("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_arff/DataDimensions.csv",
#                  sep = ",", fill = TRUE)
# datasets=as.character(datos[c(-1,-16,-27),1])

# Train
# Leemos todos los ficheros de entrenamiento por dimensión y le damos el formato deseado, añadiendo un tsIndex y un tsDim (indicador)
# de dimension. tsIndex, tsDim, tsClass, Valores
# DuckDuckGeese empty 271 test file
# FaceDetection and InsectWingBeat have 1 file only. (1 dimensional time series)
traintest="TRAIN"
UEAtoCSV<-function(workspace,dataset,traintest,outputdir){
  
  directorio=paste0(workspace,dataset)
  ficheros=list.files(directorio)
  ficheros=ficheros[grep(x=ficheros,pattern = "Dimension")]
  atributos=length(ficheros)/2
  
  out=lapply(1:atributos,function(x){
    
    data=read.arff(paste0(workspace,dataset,"/",dataset,"Dimension",x,"_",traintest,".arff"))
    
    # Fijamos el nombre para cada instante de tiempo
    names(data)[1:length(data[1,-1])]=paste0("val_",1:(length(data[1,])-1))
    
    data=cbind(tsIndex=1:length(data[,1]),tsDim=x,tsClass=data[,length(data[1,])] ,data[,-length(data[1,])])
    print(x)
    data
  })
  # rbindlist() es otra opción
  aux=do.call(rbind,out)
  
  # Por último ordenamos por tsIndex y por tsDim
  aux=aux[
    order(aux["tsIndex"], aux["tsDim"]),
    ]
  
  write.csv(aux,row.names = F,file = paste0(outputdir,"/",dataset,"_",traintest,".csv"))
}

outputdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_csv"
datasets=basename(list.dirs(workspace)[-1])

for (dataset in datasets){
  UEAtoCSV(workspace,dataset,"TRAIN",outputdir)
  UEAtoCSV(workspace,dataset,"TEST",outputdir)
}


# Formato para el algoritmo SMTS
# CSV to smts

workspace="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_csv/"
datasets=as.character(unlist(read.table("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/Selected_UEA_Datasets.txt", sep = ",")))
# dataset=datasets[4]
out=lapply(datasets,function(dataset){
  
  train=read.csv(paste0(workspace,dataset,"_TRAIN.csv"))
  test=read.csv(paste0(workspace,dataset,"_TEST.csv"))
  aux=rbind.data.frame(train,test)
  aux[,3]=as.numeric(aux[,3])
  # Mapeado de valores
  a=1:(length(unique(aux[,3])))
  names(a)=unique(aux[,3])
  aux[,3]=sapply(aux[,3],function(x){
    as.integer(a[as.character(x)])
  })
  mapped=data.frame(original=unique(aux[,3]),new=0:(length(unique(aux[,3]))-1))
  
  train=aux[1:dim(train)[1],] 
  test=aux[(dim(train)[1]+1):dim(aux)[1],] 
   
  # TRAIN
  out=lapply(unique(train[,1]), function(i){
    df_aux=train[train[,1]==i,]
    out=cbind(tsIndex=rep(i,(length(train[1,])-3)),tsTime=1:(length(train[1,])-3),tsClass=rep(df_aux[1,3],(length(train[1,])-3)),t(df_aux[,-c(1,2,3)]))
    out
  })
  out=as.data.frame(do.call(rbind,out))
  names(out)=c("tsIndex","tsTime","tsClass",paste0("Dim_",1:length(unique(aux[,2]))))
  write.table(x = out, file =paste0("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_SMTS/",
                                    dataset,"_TRAIN.txt"),
              sep = "\ " ,row.names = F, col.names = F)
  # TEST
  out=lapply(unique(test[,1]), function(i){
    df_aux=test[test[,1]==i,]
    out=cbind(tsIndex=rep(i,(length(test[1,])-3)),tsTime=1:(length(test[1,])-3),tsClass=rep(df_aux[1,3],(length(test[1,])-3)),t(df_aux[,-c(1,2,3)]))
    out
  })
  out=as.data.frame(do.call(rbind,out))
  names(out)=c("tsIndex","tsTime","tsClass",paste0("Dim_",1:length(unique(aux[,2]))))
  write.table(x = out, file =paste0("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_SMTS/",
                                    dataset,"_TEST.txt"),
              sep = "\ " ,row.names = F, col.names = F)
  
  list(dataset,mapped)
})

saveRDS(out, file = "logDePreprocesadoSMTS.RDS")

# Ejecuciones y guardado de datos del algoritmo SMTS
library(parallel)
setwd("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/")
datasetsDir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_SMTS/"
datasets=as.character(unlist(read.table("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/Selected_UEA_Datasets.txt", sep = ",")))
# dataset=datasets[15]
out=mclapply(datasets,function(dataset){
  print(dataset)
  trainfile=paste0(datasetsDir,dataset,"_TRAIN.txt")
  testfile=paste0(datasetsDir,dataset,"_TEST.txt")

  output = tryCatch({
    smts(dataset,trainfile,testfile)
  }, warning = function(w) {
    print(w)
    c(dataset,rep(NaN,11))
  }, error = function(e) {
    print(e)
    c(dataset,rep(NA,11))
  }, finally = {
    
  }
  )
  
  write.table(t(output),file="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/SMTS_results.txt",row.names = F,col.names = F,append = T,sep = ",")
  output
},mc.cores = 30)


# CSV to SD ()

workspace="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_csv/"
datasets=basename(list.files(workspace))
datasets=as.character(unlist(read.table("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/Selected_UEA_Datasets.txt", sep = ",")))
out=lapply(datasets,function(dataset){
  
  train=read.csv(paste0(workspace,dataset,"_TRAIN.csv"))
  test=read.csv(paste0(workspace,dataset,"_TEST.csv"))
  
  # Each file "datasetname_TRAIN" contains one line for every channel of every multivariate time series, together with indices for the time series and the channel index, as follows:
  #   
  # <seriesIndex I> <seriesclass Y> <seriesChannelIndex C> <seriesLength L> <the time series values, L many real values>
  #   
  # I is from 0,1,....,NumSeries-1
  # Y is from 0,1,....,NumberOfClasses-1
  # C is from 0,1,...,NumberOfChannels-1
  # Los anteriores empezaban en 0
  
  # Convertimos en numéricas todas las etiquetas. Rango 1-X por defecto en R. A continuación se le resta 1 para cumplir con el formato de SD
  # if(!is.numeric(train[,3])){
  aux=rbind.data.frame(train,test)
  aux[,3]=as.numeric(aux[,3])
  
  # Mapeado de valores
  a=1:(length(unique(aux[,3])))
  names(a)=unique(aux[,3])
  
  aux[,3]=sapply(aux[,3],function(x){
    as.integer(a[as.character(x)])
  })
  
  mapped=data.frame(original=unique(aux[,3]),new=0:(length(unique(aux[,3]))-1))
  
  aux[,c(1,2,3)]=aux[,c(1,2,3)]-1
  
  print(dataset)
  print(sum(min(aux[,1])==0))
  print(sum(min(aux[,2])==0))
  print(sum(min(aux[,3])==0))
  
  train=aux[1:dim(train)[1],] 
  test=aux[(dim(train)[1]+1):dim(aux)[1],] 
  
  outTrain=cbind.data.frame(tsIndex=train[,1],tsClass=train[,3],tsDim=train[,2],tsLen=length(train[1,-c(1,2,3)]),train[,-c(1,2,3)])
  write.table(x = outTrain, file =paste0("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_SD/",
                                         dataset,"_TRAIN.txt"),
              sep = "," ,row.names = F, col.names = F)
  
  outTest=cbind.data.frame(tsIndex=test[,1],tsClass=test[,3],tsDim=test[,2],tsLen=length(test[1,-c(1,2,3)]),test[,-c(1,2,3)])
  write.table(x = outTest, file =paste0("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_SD/",
                                        dataset,"_TEST.txt"),
              sep = "," ,row.names = F, col.names = F)
  list(dataset,mapped,sum(unique(train[,1])==0),sum(unique(train[,2])==0),sum(unique(train[,3])==0),
       sum(unique(test[,1])==0),sum(unique(test[,2])==0),sum(unique(test[,3])==0))
})

saveRDS(out, file = "logDePreprocesadoSD.RDS")

# Ejecución y guardado de datos del algoritmo SD
setwd("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/")

datasetsDir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_SD/"

datasets=as.character(unlist(read.table("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/Selected_UEA_Datasets.txt", sep = ",")))
dataset=datasets[4]
out=lapply(datasets,function(dataset){
  print(dataset)
  trainFile=paste0(datasetsDir,dataset,"_TRAIN.txt")
  testFile=paste0(datasetsDir,dataset,"_TEST.txt")
  
  command=paste0("java -jar /home/fjbaldan/01_LocalWorkSpace/0_Multivariate/Grabocka2016FastClassification/executableMultivariate.jar ",
                 "trainFile=",trainFile," testFile=",testFile," paaRatio=0.125 percentile=35 numTrials=5")
  
  output = tryCatch({
    a=system(command,intern = TRUE)
    # The format of the last output line is
    # <paaRatio> <percentile> <numTrials> <meanError> <stdError> <meanTrainTime> <stdTrainTime> <meanTotalTime> <stdTotalTime> <meanNumAcceptedShapelets> <stdNumAcceptedShapelets> 
    out=as.numeric(strsplit(tail(a,n=1),split=", ")[[1]])
    out=c(dataset,out)
    # names(output)=c("dataset","paaRatio","percentile","numTrials","meanError","stdError",
    #                 "meanTrainTime","stdTrainTime","meanTotalTime","stdTotalTime",
    #                 "meanNumAcceptedShapelets","stdNumAcceptedShapelets")
    out
  }, warning = function(w) {
    print(w)
    c(dataset,rep(NaN,11))
  }, error = function(e) {
    print(e)
    c(dataset,rep(NA,11))
  }, finally = {
    
  }
  )
  
  write.table(t(output),file="SD_results.txt",row.names = F,col.names = F,append = T,sep = ",")
  output
})

# java -jar executableMultivariate.jar trainFile=/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_SD/ArticularyWordRecognition_TRAIN.txt testFile=/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_SD/ArticularyWordRecognition_TEST.txt paaRatio=0.125 percentile=35 numTrials=5

# Tabla de datos final:
smts=read.csv(file="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/SMTS_results.txt")
sd=read.csv(file="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/SD_results.txt")

out=cbind.data.frame(smts[,1],1-smts[,2],1-sd[,5])
names(out)=c("datasets","smts_acc","sd_acc")

write.csv(out,file="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/ACC_results.csv",row.names = F)


# CMFTS
library(ccmfbd)
test=as.data.frame(t(c(1,2,3,4,5,6,7)))
a=ccmfbd(test)

csvdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_csv/"
outputdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_CMFTS/"
datasets=as.character(unlist(read.table("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/Selected_UEA_Datasets.txt", sep = ",")))
datasets=datasets[-(1:6)]

sapply(datasets,function(dataset){
  set.seed(26)
  trainFile=paste0(csvdir,dataset,"_TRAIN.csv")
  testFile=paste0(csvdir,dataset,"_TEST.csv")
  
  train=read.csv(trainFile)
  test=read.csv(testFile)
  
  # Calculo de las CM y unión en una misma instancia
  aTrain=ccmfbd(train[,-c(1,2,3)])
  aTest=ccmfbd(test[,-c(1,2,3)])
  
  # Etiquetas
  aTrain=cbind.data.frame(train[,c(1,2,3)], aTrain)
  aTest=cbind.data.frame(test[,c(1,2,3)], aTest)
  
  # Union en una instancia
  data=aTrain
  dim=length(unique(data[,2]))
  dim(data)[2]
  
  out=lapply(unique(data[,1]),function(x){
    df=data[data[,1]==x,]
    df2=data[data[,1]==x,-c(1,2,3)]
    unlist(c(df[1,c(1,3)],t(matrix(matrix(t(df2),ncol = dim),ncol=1))))
  })
  out=as.data.frame(do.call(rbind,out))
  outputNames=unlist(lapply(1:dim,function(x){
    paste0(names(data[,-c(1,2,3)]),"_",x)
  }))
  names(out)=c("tsIndex","tsClass",outputNames)
  
  write.csv(out,file = paste0(outputdir,dataset,"_TRAIN.csv"),row.names = F)
  
  data=aTest
  dim=length(unique(data[,2]))
  dim(data)[2]
  
  out=lapply(unique(data[,1]),function(x){
    df=data[data[,1]==x,]
    df2=data[data[,1]==x,-c(1,2,3)]
    unlist(c(df[1,c(1,3)],t(matrix(matrix(t(df2),ncol = dim),ncol=1))))
  })
  out=as.data.frame(do.call(rbind,out))
  outputNames=unlist(lapply(1:dim,function(x){
    paste0(names(data[,-c(1,2,3)]),"_",x)
  }))
  names(out)=c("tsIndex","tsClass",outputNames)
  
  write.csv(out,file = paste0(outputdir,dataset,"_TEST.csv"),row.names = F)
})

# Experimentacion propia con RF y C5.0

# datasets=datasets[1:4]
library(ccmfbd)
library(data.table)
library(gtools)
library(parallel)
csvdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_csv/"
outputdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_CMFTS/"
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
  
  library(C50)
  library(randomForest)
  
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
  
  # Quitamos columnas sin informacion
  colQuitar=which(apply(train,2,function(x){
    length(unique(x))<=1
  }))

  train=train[,-colQuitar]
  test=test[,-colQuitar]
  
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

write.table(out,file="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/CMFTS_ColSinInformacion_results.csv",sep=",",row.names = F)

a=read.csv("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/CMFTS_results.csv")
b=read.csv("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/ACC_results.csv")


# Modelo general:
library(ccmfbd)
csvdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_csv/"
outputdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_CMFTS/"
datasets=as.character(unlist(read.table("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/Selected_UEA_Datasets.txt", sep = ",")))

library(parallel)
# results=mclapply(datasets,function(dataset){
results=mclapply(datasets,function(dataset){
  print(dataset)
  train=read.csv(file = paste0(outputdir,dataset,"_TRAIN.csv"))
  test=read.csv(file = paste0(outputdir,dataset,"_TEST.csv"))
  
  all=rbind(train,test)
  all[,2]=as.factor(all[,2])
  train=all[1:dim(train)[1],]
  test=all[(dim(train)[1]+1):dim(all)[1],]
  
  # Todos numéricos, incluso los NA
  train[,-c(1,2)]=apply(train[,-c(1,2)],c(1,2),as.numeric)
  test[,-c(1,2)]=apply(test[,-c(1,2)],c(1,2),as.numeric)
  
  library(C50)
  library(randomForest)
  
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
  for(i in 1:ncol(train[,-c(1,2)])){
    train[,-c(1,2)][is.na(train[,-c(1,2)][,i]), i] <- mean(train[,-c(1,2)][,i], na.rm = TRUE)
  }
  
  for(i in 1:ncol(test[,-c(1,2)])){
    test[,-c(1,2)][is.na(test[,-c(1,2)][,i]), i] <- mean(test[,-c(1,2)][,i], na.rm = TRUE)
  }
  
  # El dataset de test ha de tener las mismas columnas que el dataset de entrada,
  # si la variable se ha eliminado por todo Na NaN se rellena a 0
  newTest=data.frame(matrix(0, nrow=dim(test)[1], ncol=dim(train)[2]))
  names(newTest)=names(train)
  sel=names(train)[names(train)%in%names(test)]
  for(col in sel){
    newTest[,col]=test[,col]
  }
  test=newTest
  
  # Busco el número de dimensiones de cada problema y lo convierto en los datos originales
  # grepl(x=names(train),pattern =  "_[0-9]")
  dim=as.numeric(tail(strsplit(tail(names(train),n=1),split = "_")[[1]],n=1))
  trainO=NULL
  testO=NULL
  auxS=NULL
  for (i in 1:dim){
    
    #Extraemos la dimensión
    sel=sapply(strsplit(names(train),split = "_"), function(x){
      out=as.numeric(tail(x,n=1))
      if(is.na(out))out=FALSE
      out
      })==i
    
    if(sum(sel)>=1){
      aux=cbind(train[,c(1,2)],tsDim=i,train[,sel])
      
      print(i)
      print(sum(sel))
      if(i>1)names(aux)=names(trainO) #ERROR AQUI, el grep encuentra multiples variables, si pones "_1" buca tmb "_10" "_11" etc
      trainO=rbind(trainO,aux)
      
      sel=sapply(strsplit(names(test),split = "_"), function(x){
        out=as.numeric(tail(x,n=1))
        if(is.na(out))out=FALSE
        out
      })==i
      
      aux=cbind(test[,c(1,2)],tsDim=i,test[,sel])
      if(i>1) names(aux)=names(testO)
      testO=rbind(testO,aux)
    }
      
  }
  
  test=testO
  train=trainO
  
  train=train[
    order(train["tsIndex"],train["tsDim"]),
    ]
  
  test=test[
    order(test["tsIndex"], test["tsDim"]),
    ]
  
  output = tryCatch({
    model=C5.0(x=train[,-c(1,2,3)],y = train[,2],trials = 10)
    pred=predict(model,newdata=test[,-c(1,2,3)])
    
    testLabels=matrix(sapply(test["tsClass"],function(x){as.numeric(x)}),ncol = dim,byrow = T)[,1]
    predLabels=matrix(sapply(pred,function(x){as.numeric(x)}),ncol = dim,byrow = T)
    
    # Buscamos que clase tiene mas votos, en caso de empate,
    # se hace un random de los resultados y se toma el mayor que aparezca primero
    pred=apply(predLabels,1,function(x){
      vote=table(x)
      vote=vote[sample(nrow(vote))]
      names(which.max(vote))
    })
    
    accC5=mean(pred==testLabels)
    accC5
    
    
    model=randomForest(tsClass ~ .,train[,c(-1,-3)])
    pred=predict(model,newdata=test[,-c(1,2,3)])
    
    testLabels=matrix(sapply(test["tsClass"],function(x){as.numeric(x)}),ncol = dim,byrow = T)[,1]
    predLabels=matrix(sapply(pred,function(x){as.numeric(x)}),ncol = dim,byrow = T)
    
    # Buscamos que clase tiene mas votos, en caso de empate,
    # se hace un random de los resultados y se toma el mayor que aparezca primero
    pred=apply(predLabels,1,function(x){
      vote=table(x)
      vote=vote[sample(nrow(vote))]
      names(which.max(vote))
    })
    
    accRF=mean(pred==testLabels)
    accRF
    
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

write.table(out,file="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/CMFTS_1modelVoteAtribute_3_results.csv",sep=",",row.names = F)





# 1 Modelo por variable:
# Modelo general:
library(ccmfbd)
csvdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_csv/"
outputdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_CMFTS/"
datasets=as.character(unlist(read.table("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/Selected_UEA_Datasets.txt", sep = ",")))

# datasets=datasets[c(6,17,19)]
datasets=datasets[-c(19)]

library(parallel)
# results=mclapply(datasets,function(dataset){
results=mclapply(datasets,function(dataset){
  print(dataset)
  train=read.csv(file = paste0(outputdir,dataset,"_TRAIN.csv"))
  test=read.csv(file = paste0(outputdir,dataset,"_TEST.csv"))
  
  all=rbind(train,test)
  all[,2]=as.factor(all[,2])
  train=all[1:dim(train)[1],]
  test=all[(dim(train)[1]+1):dim(all)[1],]
  
  # Todos numéricos, incluso los NA
  train[,-c(1,2)]=apply(train[,-c(1,2)],c(1,2),as.numeric)
  test[,-c(1,2)]=apply(test[,-c(1,2)],c(1,2),as.numeric)
  
  library(C50)
  library(randomForest)
  
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
  for(i in 1:ncol(train[,-c(1,2)])){
    train[,-c(1,2)][is.na(train[,-c(1,2)][,i]), i] <- mean(train[,-c(1,2)][,i], na.rm = TRUE)
  }
  
  for(i in 1:ncol(test[,-c(1,2)])){
    test[,-c(1,2)][is.na(test[,-c(1,2)][,i]), i] <- mean(test[,-c(1,2)][,i], na.rm = TRUE)
  }
  
  # El dataset de test ha de tener las mismas columnas que el dataset de entrada,
  # si la variable se ha eliminado por todo Na NaN se rellena a 0
  newTest=data.frame(matrix(0, nrow=dim(test)[1], ncol=dim(train)[2]))
  names(newTest)=names(train)
  sel=names(train)[names(train)%in%names(test)]
  for(col in sel){
    newTest[,col]=test[,col]
  }
  test=newTest
  
  # Busco el número de dimensiones de cada problema y lo convierto en los datos originales
  # grepl(x=names(train),pattern =  "_[0-9]")
  dim=as.numeric(tail(strsplit(tail(names(train),n=1),split = "_")[[1]],n=1))
  trainO=NULL
  testO=NULL
  auxS=NULL
  for (i in 1:dim){
    
    #Extraemos la dimensión
    sel=sapply(strsplit(names(train),split = "_"), function(x){
      out=as.numeric(tail(x,n=1))
      if(is.na(out))out=FALSE
      out
    })==i
    
    if(sum(sel)>=1){
      aux=cbind(train[,c(1,2)],tsDim=i,train[,sel])
      
      print(i)
      print(sum(sel))
      if(i>1)names(aux)=names(trainO) #ERROR AQUI, el grep encuentra multiples variables, si pones "_1" buca tmb "_10" "_11" etc
      trainO=rbind(trainO,aux)
      
      sel=sapply(strsplit(names(test),split = "_"), function(x){
        out=as.numeric(tail(x,n=1))
        if(is.na(out))out=FALSE
        out
      })==i
      
      aux=cbind(test[,c(1,2)],tsDim=i,test[,sel])
      if(i>1) names(aux)=names(testO)
      testO=rbind(testO,aux)
    }
    
  }
  
  test=testO
  train=trainO
  
  train=train[
    order(train["tsIndex"],train["tsDim"]),
    ]
  
  test=test[
    order(test["tsIndex"], test["tsDim"]),
    ]
  
  output = tryCatch({
    
    dimPred=lapply(1:dim, function(d){
      sel=(train[,3]==d)
      dimtrain=train[sel,]
      sel=(test[,3]==d)
      dimtest=test[sel,]
      model=C5.0(x=dimtrain[,-c(1,2,3)],y = dimtrain[,2],trials = 10)
      pred=predict(model,newdata=dimtest[,-c(1,2,3)])
    })
    
    predLabels=do.call(cbind,dimPred)
    
    testLabels=matrix(sapply(test["tsClass"],function(x){as.numeric(x)}),ncol = dim,byrow = T)[,1]
    # predLabels=matrix(sapply(pred,function(x){as.numeric(x)}),ncol = dim,byrow = T)
    
    # Buscamos que clase tiene mas votos, en caso de empate,
    # se hace un random de los resultados y se toma el mayor que aparezca primero
    pred=apply(predLabels,1,function(x){
      vote=table(x)
      vote=vote[sample(nrow(vote))]
      names(which.max(vote))
    })
    
    accC5=mean(pred==testLabels)
    accC5
    
    # pred1=pred
    
    dimPred=lapply(1:dim, function(d){
      sel=(train[,3]==d)
      dimtrain=train[sel,]
      sel=(test[,3]==d)
      dimtest=test[sel,]
      model=randomForest(tsClass ~ .,dimtrain[,c(-1,-3)])
      pred=predict(model,newdata=dimtest[,-c(1,2,3)])
    })
    
    predLabels=do.call(cbind,dimPred)
    
    testLabels=matrix(sapply(test["tsClass"],function(x){as.numeric(x)}),ncol = dim,byrow = T)[,1]
    # predLabels=matrix(sapply(pred,function(x){as.numeric(x)}),ncol = dim,byrow = T)
    
    # Buscamos que clase tiene mas votos, en caso de empate,
    # se hace un random de los resultados y se toma el mayor que aparezca primero
    pred=apply(predLabels,1,function(x){
      vote=table(x)
      vote=vote[sample(nrow(vote))]
      names(which.max(vote))
    })
    
    accRF=mean(pred==testLabels)
    accRF
    pred2=pred
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

write.table(out,file="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/CMFTS_NmodelVoteBYAtribute_results.csv",sep=",",row.names = F)









