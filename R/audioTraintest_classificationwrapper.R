classifierWrapper<-function(X,Y,k,testSample)
{
  R=-1
  P=-1
  ret=list(R=maxarg,P=P)
  ret=classify(X,Y,k,testSample)
  
  return (ret)
}

trainKNN<-function(features,k)
{
  X=-1
  Y=-1
  l=list(X=X,Y=Y)
  l=listofFeatures2Matrix(features)
  return (l)
  
}


listOfFeatures2Matrix<-function(features)
{
 
  X=list()
  Y=c()
  i=0
  for (f in features)
  {
    if(i==0)
    {
      X=f
      Y=c(Y,i*matrix(1,NROW(f),1))
    }
    else
    {
      print("hello")
      X=list(X,f)
      #print(X)
      #X=t(X)
      #print(X)
      Y=c(Y,i*matrix(1,NROW(f),1))
      Y=t(Y)
    }
    i=i+1
  }
  
    returnlistOfFeatures2Matrix=list(X=X,Y=Y)
    return (returnlistOfFeatures2Matrix)
  }
    

x=matrix(c(1,2,3,4,5,6),ncol=2)
z=matrix(c(11,12,13,14,15,16),ncol=2)
w=matrix(c(21,22,23,24,25,26),ncol=2)
features=list(x,z,w)
returnlistOfFeatures2Matrix=list(X=X,Y=Y)
returnlistOfFeatures2Matrix=listOfFeatures2Matrix(features)
print(returnlistOfFeatures2Matrix$X)
print(returnlistOfFeatures2Matrix$Y)


randSplitFeatures<-function(features,partTrain)
{
  
  featuresTrain=list()
  featuresTest=list()
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
x=matrix(c(1,2,3,4,5,6),ncol=2)
z=matrix(c(11,12,13,14,15,16),ncol=2)
w=matrix(c(21,22,23,24,25,26),ncol=2)
features=list(x,z,w)
partTrain=0.60
returnofrandSplitFeatures=list(featuresTrain=featuresTrain,featuresTest=featuresTest)
returnofrandSplitFeatures=randSplitFeatures(features,partTrain)

print(returnofrandSplitFeatures$featuresTrain)
print(returnofrandSplitFeatures$featuresTest)




evaluateClassifier<-function(features,ClassNames,nExp,Params,parameterMode,perTrain=0.90)
{
  #check return tye variables in list
  #returnnormalizefeatures=list(featuresNorm=featuresNorm,meanreturn=meanreturn,stdreturn=stdreturn)=normalizeFeatures(features)
  nClasses=length(features)
  CALL=c()
  acALL=c()
  F1ALL=c()
  PrecisionClassesAll=c()
  RecallClassesAll=c()
  ClassesAll=c()
  F1ClassesAll=c()
  CMsAll=c()
  nSampleTotal=0
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
    CM=matrix(0,nClasses,nClasses)
    for (e in seq(1,nExp))
    { #print "Param = {0:.5f} - Classifier Evaluation Experiment {1:d} of {2:d}".format(C, e+1, nExp)
      retournofrandSplitFeatures=list(featuresTrain=featuresTrain,featuresTest=featuresTest)
      returnofrandSplitFeatures=randSplitFeatures(featuresNorm,perTrain)
      returnlistOfFeatures2Matrix=list(X=X,Y=Y)
      returnlistOfFeatures2Matrix=trainKNN(featuresTrain,C)
      CMt=matrix(0,nClasses,nClasses)
      for (c1 in seq(1,nClasses))
      {
        nTestSamples=NROW(featuresTest[c1])#check NROW or length
        Results=matrix(0,nTestSamples,1)
        for (ss in seq(1,nTestSamples))
        {
          #continue
        }
        }
    }
  }
}





































