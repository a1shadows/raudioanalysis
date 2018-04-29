classifierWrapper<-function(X,Y,k,testSample)
{
  R=-1
  P=-1
  ret=list(R=maxarg,P=P)
  ret=classify(X,Y,k,testSample)
  
  return (ret)
}