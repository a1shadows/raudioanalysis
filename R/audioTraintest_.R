
source("audioFeatureExtraction.R")
library(pdist)
featureAndTrain<-function(listOfDirs, mtWin, mtStep, stWin, stStep, modelName="knn", computeBEAT=FALSE, perTrain=0.90) 
{
  k=list()
  k=dirsWavFeatureExtraction(listOfDirs, mtWin, mtStep, stWin, stStep, computeBEAT=computeBEAT)
  classNames=k[[2]]
  features=k[[1]]
  print(k[[1]])
  if(length(features)==0)
  {
    print ("trainSVM_feature ERROR: No data found in any input folder!")
  }
  numOfFeatures=NCOL(features)
  featureNames<-c()
  for (i in 1:numOfFeatures)
  {
    featureNames[i]=paste("features",i)
  }
  writeTrainDataToARFF(modelName,k[[1]], classNames, featureNames)
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
 
  #features = features2
  #print(features)
  #print("FEATURES 2:")
  #print(features)
  bestParam = evaluateClassifier(k[[1]], classNames, 100, classifierType, classifierParams, 0)
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
    
    cat(paste(classNames[c]), file=x, append=TRUE, sep = "\n")
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
classify<-function(X,Y,k,testSample)
{
  
  nClasses1=unique(Y)
  nClasses=NROW(nClasses1)
  i=NCOL(testSample)
  n=nrow(X)
  m=nrow(Y)
  Yi=matrix(testSample,nrow=1,ncol=i)
  Ydist <- pdist(X, Yi)
  Ydist=as.matrix(Ydist)
  
  
  iSort=order(Ydist)
  iSort
  P=matrix(0,nClasses,1)
  P
  y=iSort
  for(i in seq(1,nClasses))
  {
    print(Y[1:k,y])
    z=which(Y[1:k,y]==i,arr.ind=T)
    print(z)
    z1=NCOL(z)
    P[i]=z1/k
    print(P[i])
  }
  maxarg=which.max(P)
  l=list(maxarg=maxarg,P=P)
  return (l)
  
}

classifierWrapper<-function(X,Y,k,testSample)
{
  maxarg=-1
  P=-1
  retclassifierWrapper=list()
  retclassifierWrapper=classify(X,Y,k,testSample)
  
  return (retclassifierWrapper)
}
listOfFeatures2Matrix<-function(features)
{
  
  X=c()
  Y=c()
  i=0
  for (f in features)
  {
    if(i==0)
    {
      X=f
      Y=rbind(Y,i*matrix(1,NROW(f),1))
    }
    else
    {
      print("hello")
      X=rbind(X,f)
      Y=c(Y,i*matrix(1,NROW(f),1))
    }
    i=i+1
  }
  
  returnlistOfFeatures2Matrix=list(X=X,Y=Y)
  return (returnlistOfFeatures2Matrix)
}

trainKNN<-function(features,k)
{
  X=-1
  Y=-1
  l=list()
  l=listOfFeatures2Matrix(features)
  return (l)
  
}



# x=matrix(c(1,2,3,4,5,6),ncol=2)
# z=matrix(c(11,12,13,14,15,16),ncol=2)
# w=matrix(c(21,22,23,24,25,26),ncol=2)
# features=list(x,z,w)
# returnlistOfFeatures2Matrix=list()
# returnlistOfFeatures2Matrix=listOfFeatures2Matrix(features)
# print(returnlistOfFeatures2Matrix$X)
# print(returnlistOfFeatures2Matrix$Y)



randSplitFeatures<-function(features,partTrain)
{
  x=matrix(c(-1,-1,-1,-1,-1,-1),ncol=2)
  z=matrix(c(0,0,0,0,0,0),ncol=2)
  w=matrix(c(1,1,1,1,1,1),ncol=2)
  features=list(x,z,w)
  featuresTrain=list()
  featuresTest=list()
  partTrain=0.60
  for (f in features)
    {
    numOfSamples=NROW(f)
    numOfDims=NCOL(f)
    b=seq(1,numOfSamples)
    randperm=sample(b,length(b),replace=FALSE)
    nTrainSamples=round(partTrain*numOfSamples)
    k1=c()
    k1=randperm[1:nTrainSamples]
    i1=c()
    i1=f[k1,1:numOfDims]
    featuresTrain=list(featuresTrain,i1)
    updatednTrainSamples=nTrainSamples+1
    k2=c()
    randpermlen=length(randperm)
    k2=randperm[updatednTrainSamples:randpermlen]
    i2=c()
    i2=f[k2,1:numOfDims]
    featuresTest=list(featuresTest,i2)
  }
  retournofrandSplitFeatures=list(featuresTrain=featuresTrain,featuresTest=featuresTest)
  return (retournofrandSplitFeatures)
  
}
# x=matrix(c(1,2,3,4,5,6),ncol=2)
# z=matrix(c(11,12,13,14,15,16),ncol=2)
# w=matrix(c(21,22,23,24,25,26),ncol=2)
# features=list(x,z,w)
# partTrain=0.60
# returnofrandSplitFeatures=list(featuresTrain=featuresTrain,featuresTest=featuresTest)
# returnofrandSplitFeatures=randSplitFeatures(features,partTrain)
# 
# print(returnofrandSplitFeatures$featuresTrain)
# print(returnofrandSplitFeatures$featuresTest)




evaluateClassifier<-function(features,ClassNames,nExp,Params,parameterMode,perTrain=0.90)
{
 

  # perTrain=0.60
  # parameterMode=0
  # x=matrix(c(1,2,3,4,5,6),ncol=2)
  # z=matrix(c(11,12,13,14,15,16),ncol=2)
  # w=matrix(c(21,22,23,24,25,26),ncol=2)
  # Params =c(1, 3, 5, 7, 9, 11, 13, 15)  
  # ClassNames=c('abc','bcd','pqr')
  # features=list(x,z,w)
  returnnormalizefeatures=list()
  print(features)
  returnnormalizefeatures=normalizeFeatures(features)
  featuresNorm=returnnormalizefeatures$featuresNorm
  nClasses=length(features)
  CALL=c()
  acAll=c()
  F1All=c()
  PrecisionClassesAll=c()
  RecallClassesAll=c()
  ClassesAll=c()
  F1ClassesAll=c()
  CMsAll=c()
  nSampleTotal=0
  nExp=100
  for (f in features)
    {
    nSampleTotal=nSampleTotal+NROW(f)
  }
  if(nSampleTotal>1000 & nExp>50 )
  {
    nExp=50
    print("Number of training experiments changed to 50 due to high number of samples")
  }
  if(nSampleTotal>2000 & nExp>10)
  {
    nExp=10
    print("Number of training experiments changed to 10 due to high number of samples")
  }
  for (C in Params)
  {
    C1=C
    print(C1)
    CM=matrix(0,nClasses,nClasses)
    for (e in seq(1,nExp))
    { 
      print(cat("Param =",C,"- Classifier Evaluation Experiment",(e+1),"of",nExp))
      retournofrandSplitFeatures=list(featuresTrain=featuresTrain,featuresTest=featuresTest)
      returnofrandSplitFeatures=randSplitFeatures(featuresNorm,perTrain)
      #as.numeric(unlist(returnofrandSplitFeatures$featuresTrain))
      returnlistOfFeatures2Matrix=list()
      featuresTrain=returnofrandSplitFeatures$featuresTrain
      returnlistOfFeatures2Matrix=trainKNN(featuresTrain,C)
      featuresTest=returnofrandSplitFeatures$featuresTest
      CMt=matrix(0,nClasses,nClasses)
      for (c1 in seq(1,nClasses))
      {
        nTestSamples=NROW(featuresTest[[c1]])
        Results=matrix(0,nTestSamples,1)
        retclassifierWrapper=list()
      
        for (ss in seq(1,nTestSamples))
        {
          
          featurestosend=features[[c1]]
          #print(featurestosend)
          featurestosend1=featurestosend[ss,1:NCOL(featurestosend)]
          #print(featurestosend1)
          #print(C)
          X=returnlistOfFeatures2Matrix$X
          Y=returnlistOfFeatures2Matrix$Y
          retclassifierWrapper=classifierWrapper(X,Y,C,featurestosend1)
          Results[ss]=retclassifierWrapper$maxarg
        }
        for (c2 in seq(1,nClasses))
        {
          CMt[c1,c2]=length(which(Results==c2,arr.ind = TRUE))
        }
      }
    }
      CM+CM+CMt
      CM=CM+0.0000000010
      Rec=matrix(0,NROW(CM),1)
      Pre=matrix(0,NROW(CM),1)
      for (ci in seq(1,NROW(CM)))
      {
        Rec[ci]=CM[ci,ci]/sum(CM[ci,1:NCOL(CM)])
        Pre[ci]=CM[ci,ci]/sum(CM[1:NROW(CM),ci])
      }
      PrecisionClassesAll=c(PrecisionClassesAll,Pre)
      RecallClassesAll=c(RecallClassesAll,Rec)
      F1=(2*Rec*Pre)/(Rec+Pre)
      F1ClassesAll=c(F1ClassesAll,F1)
      l1=sum(diag(CM))
      l2=l1/sum(CM)
      acAll=c(acAll,l2)
      CMsAll=c(CMsAll,CM)
      F1All=c(F1All,mean(F1))
      
    
  }
  print("\t\t")
  i=1
  for (c in ClassNames)
  {
    if(i==length(ClassNames)-1)
    {
      print(cat(c,"\t\t"))
    }
    else
    {
      print(cat(c,"\t\t"))
    }
    print("OVERALL")
    print("\tC")
    for (c in ClassNames)
    {
      print("\tPRE\tREC\tF1")
    }
    print(cat("\t","ACC","\t","F1"))
    bestAcInd=which.max(acAll)
    bestF1Ind=which.max(F1All)
    for (i in seq(1,length(PrecisionClassesAll)))
    {
      print(cat("\t",format(round(Params[i],3))))
      for (c in seq(1,length(PrecisionClassesAll[i])))
      {
        print(cat("\t",round((100.0 * PrecisionClassesAll[i][c]),1),"\t",round((100.0 * RecallClassesAll[i][c]),1),"\t",round((F1ClassesAll[i][c]),1)))
      }
      print(cat("\t",round((100.0 * acAll[i]),1),"\t",round((100.0 * F1All[i]),1)))
      if(i==bestF1Ind)
      {
        print("\tbest\t")
      }
      if(i==bestAcInd)
      {
        print("\tbest\t")
      }
    }
    if(parameterMode==0)
    {
      return (Params[bestAcInd])
    }
    else if(parameterMode==1)
    {
      return (Params[bestF1Ind])
    }
  }
}



  




normalizeFeatures<-function(features)
{
  X=array()
  i=1
  for (i in seq(1,NCOL(features)))
  {
    f=features[1:NROW(features),i]
    if(NROW(f)>0)
    {
      if(i==1)
      {
        X=f
      }
      else
      {
        
        X=rbind(X,f)
        print(X)
      }
      i=i+1
    }
  }
  #print("value of X is:")
  #print(X)
  MEAN=colMeans(X, na.rm = FALSE, dims = 1)+0.00000000000001
  STD=apply(X,2,sd)+0.00000000000001
  featuresNorm=list()
  o=1
  for (i in seq(1,NCOL(features)))
  {
    f=features[1:NROW(features),i]
    
    ft=f
    print("ft is")
    print(ft)
    ft[o,1:NCOL(ft)]=(trunc((ft[o,1:NCOL(ft)]-MEAN)/STD))
      
    featuresNorm=list(featuresNorm,ft)
    o=o+1
  }
  returnnormalizeFeatures=list(featuresNorm=featuresNorm,MEAN=MEAN,STD=STD)
  return (returnnormalizeFeatures)
}



# x=matrix(c(1,4,2,5,3,6),nrow=2)
# z=matrix(c(11,14,12,15,13,16),nrow=2)
# w=matrix(c(21,24,22,25,23,26),nrow=2)
# features=list(x,z,w)
# returnnormalizeFeatures=list()
# returnnormalizeFeatures=normalizeFeatures(features)
# print(returnnormalizeFeatures$featuresNorm)
# print(returnnormalizeFeatures$MEAN)
# print(returnnormalizeFeatures$STD)



