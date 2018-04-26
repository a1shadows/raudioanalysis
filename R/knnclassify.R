library(pdist)
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
  l=list()
  l$maxarg=maxarg
  l$P=P
  return (l)
  
}

nClasses=10
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
X<- array(c(vector1,vector2),dim = c(3,3))
X=t(X)
vect1 <- c(5,91,3)
vect2 <- c(1,11,2,1,4,1)
Y<- array(c(vect1,vect2),dim = c(3,3))
Y
v1 <- c(1,2,3)
#v2 <- c(100,110,120,130,140,150)
k=2
testSample<- array(c(v1),dim = c(1,3))
ret=list(maxarg=maxarg,P=P)
ret=classify(X,Y,k,testSample)
print(ret$maxarg)
print(ret$P)



