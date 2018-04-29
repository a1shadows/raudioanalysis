classifierWrapper<-function(X,Y,k,testSample)
{
  R=-1
  P=-1
  retclassifierWrapper=list(R=maxarg,P=P)
  retclassifierWrapper=classify(X,Y,k,testSample)
  
  return (retclassifierWrapper)
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
  x=matrix(c(1,2,3,4,5,6),ncol=2)
  z=matrix(c(11,12,13,14,15,16),ncol=2)
  w=matrix(c(21,22,23,24,25,26),ncol=2)
  Params =c(1, 3, 5, 7, 9, 11, 13, 15)  
  ClassNames=c("abc","bcd","pqr")
  features=list(x,z,w)
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
  nExp=20
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
        retclassifierWrapper=list()
        retclassifierWrapper=list(R=maxarg,P=P)
        for (ss in seq(1,nTestSamples))
        {
          retclassifierWrapper=classifierWrapper(returnlistOfFeatures2Matrix$X,returnlistOfFeatures2Matrix$y,C,features[c1,ss])
          Results[ss]=retclassifierWrapper$R
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
      F1ALL=c(F1ALL,mean(F1))
      
    
  }
  print("\t\t")
  i=1
  for (c in ClassNames)
  {
    if(i==length(ClassNames)-1)
    {
      #print("{0:s}\t\t".format(c))
    }
    else
    {
      #print("{0:s}\t\t".format(c))
    }
    print("OVERALL")
    print("\tC")
    for (c in ClassNames)
    {
      print("\tPRE\tREC\tF1")
    }
    #print("\t{0:s}\t{1:s}".format("ACC", "F1"))
    bestAcInd=which.max(acALL)
    bestF1Ind=which.max(F1ALL)
    for (i in seq(1,length(PrecisionClassesAll)))
    {
      # print "\t{0:.3f}".format(Params[i]),
      for (c in seq(1,length(PrecisionClassesAll[i])))
      {
        # print "\t{0:.1f}\t{1:.1f}\t{2:.1f}".format(100.0 * PrecisionClassesAll[i][c], 100.0 * RecallClassesAll[i][c], 100.0 * F1ClassesAll[i][c]),
      }
      #print "\t{0:.1f}\t{1:.1f}".format(100.0 * acAll[i], 100.0 * F1All[i]),
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
      print("Confusion Matrix:")
      printConfusionMatrix(CMsAll[bestAcInd],ClassNames)
      return (Params[bestAcInd])
    }
    else if(parameterMode==1)
    {
      print("Confusion Matrix:")
      printConfusinMatrix(CMsAll[bestF1Ind],ClassNames)
      return (Params[bestF1Ind])
    }
  }
}


printConfusionMatrix<-function(CM,ClassNames)
{
  if(NROW(CM)!=length(ClassNames))
  {
    print("printConfusionMatrix: Wrong argument sizes\n")
    return ()
  }
  for (c in ClassNames)
  {
    if(length(c)>4)
    {
      c=c[1:3]
    }
    #print "\t{0:s}".format(c),
  }
   for (c in ClassNames)
   {
     if(length(c)>4)
     {
       c=c[1:3]
     }
     #print "{0:s}".format(c),
   }
     for (j in seq(1,length(ClassNames)))
     {
       #print "\t{0:.2f}".format(100.0 * CM[i][j] / numpy.sum(CM)),
     }
   }
    
    
  




normalizeFeatures<-function(features)
{
  X=array()
  i=1
  for (f in features)
  {
    if(NROW(f)>0)
    {
      if(i==1)
      {
        X=f
      }
      else
      {
        lengthofstack=NROW(X)+NROW(f)
        print(lengthofstack)
        print(f)
        X=rbind(X,f)
        print(X)
      }
      i=i+1
    }
  }
  MEAN=colMeans(X, na.rm = FALSE, dims = 1)+0.00000000000001
  STD=apply(X,2,sd)+0.00000000000001
  featuresNorm=list()
  for (f in features)
  {
    
    ft=f
    for (nSamples in seq(1,NROW(f)))
    {
      
      ft[nSamples,seq(1,NCOL(ft))]=(trunc((ft[nSamples,seq(1,NCOL(ft))]-MEAN)/STD))
      
    }
    featuresNorm=list(featuresNorm,ft)
  }
  returnnormalizeFeatures=list(featuresNorm=featuresNorm,MEAN=MEAN,STD=STD)
  return (returnnormalizeFeatures)
}


x=matrix(c(1,4,2,5,3,6),nrow=2)
z=matrix(c(11,14,12,15,13,16),nrow=2)
w=matrix(c(21,24,22,25,23,26),nrow=2)
features=list(x,z,w)
returnnormalizeFeatures=list(featuresNorm=featuresNorm,MEAN=MEAN,STD=STD)
returnnormalizeFeatures=normalizeFeatures(features)
print(returnnormalizeFeatures$featuresNorm)
print(returnnormalizeFeatures$MEAN)
print(returnnormalizeFeatures$STD)



fileClassification<-function(inputFile,modelName)
{
  if(!file.exists(inputFile))
  {
    print("wav file not found")
  }
  if(!file.exists(modelName))
  {
    print("input model not found")
  }
  returnloadknn=list(X=X,Y=Y,k=k,MEAN=MEAN,STD=STD,classNames=classNames,mtWin=mtWin,mtStep=mtStep,stWin=stWin,stStep=stStep,computeBEAT=computeBEAT)
  returnloadknn=loadKNNModel(modelName)
  returnreadaudiofile=list(Fs=Fs,x=x)
  returnreadaudiofile=audioBasicIO.readAudioFile(inputFile)
  returnreadaudiofile$x=audioBasicIO.readAudioFile(returnreadaudiofile$x)
 # if isinstance(x, int):                               
  #  return (-1, -1, -1)
  returnnull=list(ret1=-1,ret2=-1,ret2=-1)
  if((NROW(x)/Fs)<=mtWin)
  {
    return (returnnull)
  }
  returnFeatureExtraction=list(MidTermFeatures=MidTermFeatures,s=s)
  returnFeatureExtraction=aF.mtfeatureExtraction(x,Fs,mtWin*Fs,mtStep,round(Fs * stWin), round(Fs * stStep))
  returnFeaturesExtraction$MidTermFeatures=rowMeans( returnFeaturesExtraction$MidTermFeatures=rowMeans,na.rm = FALSE, dims = 1)
  curFV=(returnFeaturesExtraction$MidTermFeatures-MEAN)/STD
  returnClassifierWrapper=list(Result=Result,P=P)
  returnClassifierWrapper=classifierWrapper(X,Y,k,curFV)
  returnoffileclassification=list(Result=Result,P=P,classNames=classNames)
  return (returnoffileclassification)
  
  
  }
