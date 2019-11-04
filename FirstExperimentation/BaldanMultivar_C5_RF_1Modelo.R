# 1 modelo
library(ccmfbd)
library(data.table)
library(gtools)
library(parallel)
library(C50)
library(randomForest)
csvdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_csv/"
outputdir="/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/UEA/Multivariate_CMFTS/"
datasets=as.character(unlist(read.table("/home/fjbaldan/01_LocalWorkSpace/0_Multivariate/Selected_UEA_Datasets.txt", sep = ",")))

library(parallel)
results=mclapply(datasets,function(dataset){
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