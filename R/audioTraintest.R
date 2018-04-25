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



