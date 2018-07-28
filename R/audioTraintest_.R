stop()
source(paste0("R",.Platform$file.sep,"audioFeatureExtraction.R"))
library(pdist)
library(kernlab)

featureAndTrain<-function(listOfDirs, mtWin, mtStep, stWin, stStep, classifierType, modelName, computeBEAT, perTrain) 
{
  if (missing(computeBEAT)){
    computeBEAT = F
  }
  if(missing(perTrain)){
    perTrain = 0.09
  }
  #loadparameters()
  k=dirsWavFeatureExtraction(listOfDirs, mtWin, mtStep, stWin, stStep, computeBEAT=computeBEAT)
  classNames=k[[2]]
  features=k[[1]]
  #print(length(features))
  #stop("pingping")
  
  numOfFeatures= dim(features[[1]])[2]
  print(numOfFeatures)
  #stop("t t ")
  featureNames<-c()
  for (i in 1:numOfFeatures)
  {
    featureNames[i]=paste0("features",i)
  }
  writeTrainDataToARFF(modelName, features, classNames, featureNames)
  #stop("testing")

  
  classifierParams<-c(1, 3, 5, 7, 9, 11, 13, 15)
  
  #features=list('a'=c(1,2,3,4),'b'=c(3,4,5,6,6,7),'c'=c(4,2,7,5,9,8,0,0,2))
  # features2<-list()
  # for (f in features)
  # {
  #   fTemp=list()
  #   for(i in 1:length(features))
  #   {
  #     y=c(lapply(features,length))
  #     for (j in 1:length(y[i]))
  #     {temp=list()
  #     temp = (features[i])
  #     if((!any(is.nan(temp)))&(!any(is.infinite(temp))))
  #       
  #     { append(fTemp,temp)}
  #     
  #     else
  #       
  #     {
  #       print ("NaN Found! Feature vector not used for training")
  #     }
  #     append(features2,fTemp)
  #     
  #     }
  #   }
  # }
  # 
  #features = features2
  #print(features)
  #print("FEATURES 2:")
  #print(features)
  bestParam = evaluateClassifier(k[[1]], classNames, 100, classifierParams, 0)
  #write the above function
  
  print ("Selected params :")
  print(format(round(bestParam, 4)))
  C = length(classNames)
  
  #give ouput in normalize features as a list
  w= normalizeFeatures(features)# normalize features function to be written
  featuresNorm=w[[1]]
  
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
  #print("te te testing")
  x<-paste0(modelName,".arff")
  #print(x)
  cat(paste('@RELATION ', modelName), file=x, append=FALSE, sep = "\n")
  
  
  for (fn in featureNames)
  {
    #print(fn)
    cat(paste('@ATTRIBUTE ', fn ,' NUMERIC'), file=x, append=TRUE, sep = "\n")
  }
  cat(paste('@ATTRIBUTE class { '), file=x, append=TRUE, sep = "")
  for (c in 1:(length(classNames) - 1))
  {
    #print(classNames[c])
    cat(paste0(classNames[c], ","), file=x, append=TRUE, sep = "")
  }
  cat(paste0(classNames[length(classNames)]), file=x, append=TRUE, sep = "")
  cat(paste('}\n'), file=x, append=TRUE, sep = "\n")
  cat(paste('@DATA'), file=x, append=TRUE, sep = "\n")
  for (i in 1:length(features)){
    for (j in 1:dim(features[[i]])[1]){
      for(k in 1:dim(features[[i]])[2]){
        #cat(i, j, k, "\n")
        cat(paste0(round(as.numeric(features[[i]][j, k]), 6), ","), file=x, append=TRUE, sep = "")
      }
      cat(paste0(classNames[i]), file=x, append=TRUE, sep = "\n")
    }
  }
  
print("Aaaa")  
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
  # print("\n---")
  # print(X)
  # print("|")
  # print(Y)
  # print("|")
  # print(k)
  # print("||")
  # print(testSample)
  nClasses=length(unique(Y))
  i=length(testSample)
  n=nrow(X)
  m=length(Y)
  Yi=matrix(testSample,nrow=1,ncol=i)
  Ydist <- pdist(X, Yi)
  Ydist=as.matrix(Ydist)
  #print(Ydist)

  iSort=order(Ydist)
  P=c()
  y=iSort
  #print(y)
  for(i in seq(1,nClasses))
  {
    #print(Y[,y])
    print(y[1:k])
    print(Y)
    #stop("lkl")
    z=which(Y[y[1:k]] == i,arr.ind=T)
    #print(z)
    #stop("yokoon");
    #z1=NCOL(z)
    P[i]=z/k
    print(P[i])
  }
  #stop("poio")
  maxarg=which.max(P)
  print(maxarg)
  #stop("ftf")
  l=list(maxarg,P)
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
  f = c()
  for(i in 1:length(features)){
    Y = c(Y, rep(i, dim(features[[i]])[1]))
    if(length(Y) == 0){
      X = matrix(features[[i]], ncol = dim(features[[i]])[2])
    }
    else
      X = matrix(rbind(Y, features[[i]]), ncol = dim(features[[i]])[2])
  }
  #print(Y)
  #stop("rer")
  # print("No of columns in Training dataset are:")
  # print(NROW(features))
  # for (i in seq(1,NROW(features)))
  # {
  #   f = c(f, rep(i, dim(features)[2]))
  #   # f=features[i,1:NCOL(features)]
  #   # print(NROW(f))
  #   # if(i==1)
  #   # #{
  #   #  
  #   #   X=f
  #   #   Y=(i)*(array(1,dim=c(NROW(f),1)))
  #   # }
  #   # else
  #   # {
  #   #   
  #   #   X=rbind(X,f)
  #   #   Y=c(Y,c((i)*(array(1,dim=c(NROW(f),1)))))
  #   #   
  #   # }
  #   # 
  # }
  # #f = t(matrix(f, nrow = dim(features)[1]))
  #print(f)
  #stop("te te testing")
  # print("X in this func")
  # print(X)
  # print("Y in this func")
  # print(Y)
  return (list(X, Y))
}

trainKNN<-function(features,k)
{
  X=-1
  Y=-1
  l=list()
  # print("Features in TrainKNN")
  # print(features[[2]])
  # print(NROW(features[[2]]))
  # print(NCOL(features[[2]]))
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
  featuresTrain=list()
  featuresTest=list()
  # for (i in seq(1,NCOL(features)))
  #   {
    #f=features[1:NROW(features),i]
    for(f in features){
      numOfSamples = dim(f)[1]
      #print(numOfSamples)
      numOfDims = dim(f)[2]
      nTrainSamples = round(partTrain * numOfSamples)
      if(nTrainSamples == numOfSamples){
        nTrainSamples = nTrainSamples - 1
      }
      #cat(nTrainSamples,numOfSamples, "\n")
      #print(matrix(f[c(sample(1:nTrainSamples, size = length(1:nTrainSamples))),], ncol = numOfDims))
      #stop("434")
      featuresTrain[[length(featuresTrain) + 1]] = matrix(f[sample(1:nTrainSamples, size = length(1:nTrainSamples)),], ncol = numOfDims)
      if(nTrainSamples == (numOfSamples - 1)){
        featuresTest[[length(featuresTest) + 1]] = matrix(f[numOfSamples,], ncol = numOfDims)
      }
      else{
      featuresTest[[length(featuresTest) + 1]] = matrix(f[sample(nTrainSamples + 1:numOfSamples, size = length(nTrainSamples + 1:numOfSamples)),], ncol = numOfDims)
      }
    }
  #print(featuresTrain)
  #print("dada")
  #stop("tete")
  
  retournofrandSplitFeatures=list(featuresTrain,featuresTest)
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
  # returnnormalizefeatures=list()
  # #print(features)
  featuresNorm=normalizeFeatures(features)
  # featuresNorm=returnnormalizefeatures$featuresNorm
  # print(featuresNorm)
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
  #nSampleTotal=NCOL(features)
  for (f in features){
    nSampleTotal=nSampleTotal+(dim(f)[1])
  }
  #print(nSampleTotal)
  #stop("te te")
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
    #print(C1)
    CM=matrix(0,nClasses,nClasses)
    for (e in seq(1,nExp))
    { 
      cat("Param =",format(C, nsmall =5),"- Classifier Evaluation Experiment",(e+1),"of",nExp, "\n")
      
      retournofrandSplitFeatures=list()
      returnofrandSplitFeatures=randSplitFeatures(featuresNorm,perTrain)
      #as.numeric(unlist(returnofrandSplitFeatures$featuresTrain))
      returnlistOfFeatures2Matrix=list()
      featuresTrain=returnofrandSplitFeatures[[1]]
      #print("Training dataset")
      #print(featuresTrain)
      returnlistOfFeatures2Matrix=trainKNN(featuresTrain,C)
      X = returnlistOfFeatures2Matrix[[1]]
      Y = returnlistOfFeatures2Matrix[[2]]
      #print(Y)
      #stop("testing 1")
      #print(returnlistOfFeatures2Matrix$X)
      featuresTest=returnofrandSplitFeatures[[2]]
      #print("yoyo")
      #print(featuresTest)
      #stop("testing 1")
      CMt=matrix(0,nClasses,nClasses)
      for (c1 in seq(1,nClasses))
      {
        nTestSamples=NROW(featuresTest[[c1]])
        Results=c()
        for(ss in 1:nTestSamples){
          #cat("\n", c1, ss, "\n")
          #stop("creepyyy")
          returning = classify(X, Y, C, featuresTest[[c1]][ss,])
          Results[ss] = returning[[1]]
          cat("|", Results[ss],"\n")
          }
        #retclassifierWrapper=list()
        # 
        # print(nTestSamples)
        # featurestosend=featuresTest[[c1]]
        # for (ss in seq(1,nTestSamples))
        # {
        #   
        #   
        #    print("featurestosend")
        #    print(featurestosend)
        #    print("n rows")
        #    print(NROW(featurestosend))
        #    print("n col")
        #    print(NCOL(featurestosend))
        #   featurestosend1=featurestosend[1:NCOL(featurestosend)]
        #   print(featurestosend1)
        #   #print(C)
        #   X=returnlistOfFeatures2Matrix$X
        #   Y=returnlistOfFeatures2Matrix$Y
        #   retclassifierWrapper=classifierWrapper(X,Y,C,featurestosend1)
        #   Results[ss]=retclassifierWrapper$maxarg
        # }
        
        for (c2 in seq(1,nClasses))
        {
          CMt[c1,c2]=length(which(Results==c2,arr.ind = TRUE))
        }
        print(CMt)
        stop("sfs")
      }
      #readline()
    }
      CM=CM+CMt
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
  for(i in 1 : length(features)){
    features[[i]] = (features[[i]] - mean(features[[i]]))/sd(features[[i]])
  }
  #print(features)
  #stop("tet tr")
    return (features)
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


featureAndTrain(c("../temp1", "../temp2"), 1.0, 1.0,shortTermWindow,shortTermStep, "knn", "knnmodel", FALSE)


