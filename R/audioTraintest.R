library(pdist)
classify<-function(X,Y,k,testSample)
{
  
  nClasses1=unique(Y)
  nClasses=NROW(nClasses1)
  i=NCOL(testSample)
  n=nrow(X)
  m=nrow(Y)
  Yi=matrix(testSample,nrow=1,ncol=i)
  D <- sqrt(matrix(rep(apply(X *X, 1, sum), n), m, n, byrow = F) + matrix(rep(apply(Y*Y, 1, sum), m), m, n, byrow = T) - 2 *X %*% t(Y))
  D
  #YD=pdist(X,Yi)
  YDT=as.matrix(YD)
  YDist=t(YDT)
  iSort=order(YDist)
  iSort
  P=matrix(0,nClasses,1)
  P
  for(i in seq(length(nClasses)))
  {
    y=iSort[1]
    print(y)
    z=which(Y[1,1:k]==i,arr.ind=T)
    z1=NROW(z)
    P[i]=z1/k
    print(P[i])
  }
  maxarg=which.max(P)
  l=list()
  l$maxarg=maxarg
  l$P=P
  return (l)
  
}

nClasses=10
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
X<- array(c(vector1,vector2),dim = c(3,3))
vect1 <- c(51,91,31)
vect2 <- c(101,111,121,131,141,151)
Y<- array(c(vect1,vect2),dim = c(3,3))
Y
v1 <- c(50,90,30)
v2 <- c(100,110,120,130,140,150)
k=3
testSample<- array(c(v1,v2),dim = c(3,3,2))
ret=list(maxarg=maxarg,P=P)
ret=classify(X,Y,k,test)
print(ret$maxarg)
print(ret$P)


library(pdist)
classify<-function(X,Y,k,testSample)
{
  
  nClasses1=unique(Y)
  nClasses=NROW(nClasses1)
  i=NCOL(testSample)
  n=nrow(X)
  m=nrow(Y)
  Yi=matrix(testSample,nrow=1,ncol=i)
  D <- sqrt(matrix(rep(apply(X *X, 1, sum), n), m, n, byrow = F) + matrix(rep(apply(Y*Y, 1, sum), m), m, n, byrow = T) - 2 *X %*% t(Y))
  D
  #YD=pdist(X,Yi)
  YDT=as.matrix(YD)
  YDist=t(YDT)
  iSort=order(YDist)
  iSort
  P=matrix(0,nClasses,1)
  P
  for(i in seq(length(nClasses)))
  {
    y=iSort[1]
    print(y)
    z=which(Y[1,1:k]==i,arr.ind=T)
    z1=NROW(z)
    P[i]=z1/k
    print(P[i])
  }
  maxarg=which.max(P)
  l=list()
  l$maxarg=maxarg
  l$P=P
  return (l)
  
}

nClasses=10
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
X<- array(c(vector1,vector2),dim = c(3,3))
vect1 <- c(51,91,31)
vect2 <- c(101,111,121,131,141,151)
Y<- array(c(vect1,vect2),dim = c(3,3))
Y
v1 <- c(50,90,30)
v2 <- c(100,110,120,130,140,150)
k=3
testSample<- array(c(v1,v2),dim = c(3,3,2))
ret=list(maxarg=maxarg,P=P)
ret=classify(X,Y,k,test)
print(ret$maxarg)
print(ret$P)


featureAndTrain<-function(listOfDirs, mtWin, mtStep, stWin, stStep, modelName, computeBEAT=False, perTrain=0.90) 
{
k=list(features=features,classnames=classnames,filenames=filenames)
k=aF.dirsWavFeatureExtraction(listOfDirs, mtWin, mtStep, stWin, stStep, computeBEAT=computeBEAT)
if(length(k$features)==0)
  {
   print ("trainSVM_feature ERROR: No data found in any input folder!")
  }
  numoffeatures=NCOL(k$features)
  featurenames<-c()
  for (i in 1:numoffeatures)
  {
    featurenames[i]=cat(" features",i)
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
  saveRDS(model, file = "model.rds")
  
 # saveRDS(model, file = "model.rds")
  #Afterwards you can use
  #loadedModel <- readRDS(model.rds)
}


#this must part left to code

fo = open(modelName, "wb")
cPickle.dump(X, fo, protocol=cPickle.HIGHEST_PROTOCOL)
cPickle.dump(Y,  fo, protocol=cPickle.HIGHEST_PROTOCOL)
cPickle.dump(MEAN, fo, protocol=cPickle.HIGHEST_PROTOCOL)
cPickle.dump(STD,  fo, protocol=cPickle.HIGHEST_PROTOCOL)
cPickle.dump(classNames,  fo, protocol=cPickle.HIGHEST_PROTOCOL)
cPickle.dump(bestParam,  fo, protocol=cPickle.HIGHEST_PROTOCOL)
cPickle.dump(mtWin, fo, protocol=cPickle.HIGHEST_PROTOCOL)
cPickle.dump(mtStep, fo, protocol=cPickle.HIGHEST_PROTOCOL)
cPickle.dump(stWin, fo, protocol=cPickle.HIGHEST_PROTOCOL)
cPickle.dump(stStep, fo, protocol=cPickle.HIGHEST_PROTOCOL)
cPickle.dump(computeBEAT, fo, protocol=cPickle.HIGHEST_PROTOCOL)
fo.close()

file.create("A")
file.append("A", "heloooooo")

logFile = "log_file.txt"
cat("This is a log file for ... ", file=logFile, append=TRUE, sep = "\n")

cat("This is a log file for ... ", file=logFile, append=TRUE, sep = "\n")

x=list('ayushi')
save(x, file = "model.rda")
l<-load("model.rda")
typeof(l)
