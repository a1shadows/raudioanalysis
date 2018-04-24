#import audioFeatureExtraction as aF

featureAndTrain<-function(listOfDirs, mtWin, mtStep, stWin, stStep, modelName, computeBEAT=False, perTrain=0.90)
{
k=list(features=features,classnames=classnames,filenames=filenames)
k=aF.dirsWavFeatureExtraction(listOfDirs, mtWin, mtStep, stWin, stStep, computeBEAT=computeBEAT)
if(length(k$features)==0)
{
  print ("trainSVM_feature ERROR: No data found in any input folder!")
  return
}
numoffeatures=NCOL(k$features)
featurenames<-c()

for (i in 1:numoffeatures)
{
  featurenames[i]=cat(" features",i)
  
}

writeTrainDataToARFF(modelName, features, classNames, featureNames)

#write this function
x=c(lapply(features,length))
for (i in 1:length(x))
{li
  
  if (x[i] ==0)
    print (cat("trainSVM_feature ERROR: ",listOfDirs[i]," folder is empty or non-existing!"))
} #check for values , error in function

classifierParams=c(1, 3, 5, 7, 9, 11, 13, 15)        

features2 = list()
for (f in features)
     {
       fTemp=list()
       for(i in NROW(features))
       {
         temp = f[i,]
       }
       
     }
  





}

m = array(1:60, dim=c(3,4,5))
l=NCOL(m)
l
k=list(a=a,b=b,l=l)
a=c(1,2)
b=c(9,10)
l=c(4,7)
print(k$b)

print(length(b))




features=list('a'=c(1,2,3,4),'b'=c(3,4,5,6,6,7),'c'=c(4,2,7,5,9,8,0,0,2))

for (f in features)
{
  fTemp=list()

  for(i in 1:length(features))
  {
    
    y=c(lapply(features,length))
    for (j in 1:length(y[i]))
    {temp=c()
    temp = (features[i])
    if((!any(is.nan(temp)))&(!any(is.infinite(temp))))
    { append(fTemp,toList(temp))}
    else
    {
        print ("NaN Found! Feature vector not used for training")
    }
    append(features2,fTemp)
         
    }
   
  }
  features = features2
    
  }
  






