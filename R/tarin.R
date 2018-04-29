featureAndTrain<-function(listOfDirs, mtWin, mtStep, stWin, stStep, modelName, computeBEAT=False, perTrain=0.90) 
{
  k=list(features=features,classnames=classnames,filenames=filenames)
  k=aF.dirsWavFeatureExtraction(listOfDirs, mtWin, mtStep, stWin, stStep, computeBEAT=computeBEAT)
  if(length(k$features)==0)
  {
    print ("trainSVM_feature ERROR: No data found in any input folder!")
  }
  numOfFeatures=NCOL(k$features)
  featureNames<-c()
  for (i in 1:numOfFeatures)
  {
    featureNames[i]=paste(" features",i)
  }
  writeTrainDataToARFF(modelName, features, classNames, featureNames)
  #write this function
  
  x<-c(lapply(features,length))
  for (i in 1:length(x))
  {
    
    if (x[i] ==0)
    {
      print (cat("trainSVM_feature ERROR: ",listOfDirs[i]," folder is empty or non-existing!"))
    } 
  }
  
  classifierParams<-c(1, 3, 5, 7, 9, 11, 13, 15)
  
  #features=list('a'=c(1,2,3,4),'b'=c(3,4,5,6,6,7),'c'=c(4,2,7,5,9,8,0,0,2))
  features2<-list()
  for (f in features)
    {
    fTemp=list()
    for(i in 1:length(features))
    {
      y=c(lapply(features,length))
      for (j in 1:length(y[i]))
      {temp=list()
      temp = (features[i])
      if((!any(is.nan(temp)))&(!any(is.infinite(temp))))
        
      { append(fTemp,temp)}
      
      else
        
      {
        print ("NaN Found! Feature vector not used for training")
      }
      append(features2,fTemp)
      
      }
    }
  }
  
  features = features2
  bestParam = evaluateClassifier(features, classNames, 100, classifierType, classifierParams, 0, perTrain)
  #write the above function
  
  print ("Selected params :")
  print(format(round(bestParam, 4)))
  C = length(classNames)
  
  #give ouput in normalize features as a list
  w= normalizeFeatures(features)# normalize features function to be written
  featuresNorm=w[1]
  
  MEAN = as.list(w[2])
  STD =as.list(w[3])
  featuresNew = featuresNorm
  
  #give ouput in listOfFeatures2Matrix as a list
  q=listOfFeatures2Matrix(featuresNew)
  X=as.list(q[1])
  Y=as.list(q[2])
  #file.create("modelName")
  save(X, file = "model.rda")
  save(Y, file = "model.rda")
  save(MEAN, file = "model.rda")
  save(STD, file = "model.rda")
  save(classNames, file = "model.rda")
  save(bestParam, file = "model.rda")
  save(mtWin, file = "model.rda")
  save(mtStep, file = "model.rda")
  save(stWin, file = "model.rda")
  save(stStep, file = "model.rda")
  
  
  # saveRDS(model, file = "model.rds")
  #Afterwards you can use
  #loadedModel <- readRDS(model.rds)
}

writeTrainDataToARFF<-function(modelName, features, classNames, featureNames)
{
  
  x<-paste(modelName,".arff")
  cat(paste('@RELATION ', modelName), file=x, append=TRUE, sep = "\n")
  
 
  for (fn in featureNames)
  {
    cat(paste('@ATTRIBUTE ', fn ,' NUMERIC'), file=x, append=TRUE, sep = "\n")
  }
  cat(paste('@ATTRIBUTE class { '), file=x, append=TRUE, sep = "\n")
  for (c in 1:length(classNames))
  {
    cat(paste(classNames[c],','), file=x, append=TRUE, sep = "\n")
  }
  cat(paste('}'), file=x, append=TRUE, sep = "\n")
  cat(paste('@ATTRIBUTE class { '), file=x, append=TRUE, sep = "\n")
  c=1
  for (i in features)
  {
    for (j in i)
    {
      cat(paste(as.numeric(round(j, 6))), file=x, append=TRUE, sep = "\n")
    
    }
    
    cat(paste(classNames[c]), file=p, append=TRUE, sep = "\n")
    c=c+1
  }
  
  
}

loadKNNModel<-function(kNNModelName, isRegression=False)
{
  load(file=kNNModelNAme)
   print(X)  
   print(Y) 
   print(MEAN) 
   print(STD) 
   print(classNames) 
   print(mtWin) 
   print(mtStep) 
   print(stWin) 
   print(stStep) 
   #PROBLEM WITH K
   X = as.array(X)
   Y = as.array(Y)
   MEAN = as.array(MEAN)
   STD = as.array(STD)
   p<-list(X=X,Y=Y, MEAN=MEAN, STD=STD, classNames=classNames, mtWin=mtWin, mtStep=mtStep, stWin=stWin, stStep=stStep)
   return (p)
}
   
}