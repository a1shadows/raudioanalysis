library("seewave")
eps = 0.00000001

#frame will have to be converted into a vector

##Time-domain audio features


stZCR <- function(frame){
  return(zcr(frame, wl = NULL))
}


stEnergy<-function(frame)
{
  
  
  return (sum(frame^2)/length(frame))
  
}





stEnergyEntropy<-function(frame)
{
  eps = 0.00000001
  numOfShortBlocks<-10
  Eol<-sum(frame^2)   
  L<-length(frame)
  subWinLength<-as.integer(floor(L/numOfShortBlocks))
  if(L!= (subWinLength * numOfShortBlocks))
  {
    
    frame<- frame[1: subWinLength * numOfShortBlocks + 1]
    
    
    
  }
  
  subWindows<-matrix(frame, subWinLength, numOfShortBlocks, byrow = TRUE)
  s<-colSums (subWindows^2, na.rm = FALSE, dims = 1)/(Eol + eps)
  Entropy<- (-sum(s * log2(s + eps)))
  return (Entropy)
}


##Frequency-domain audio features

stSpectralCentroidAndSpread<-function(X,fs)
{
  
  eps<-0.00000001
  
  ind<-array(seq(1,length(X)+1))*(fs/(2.0*length(X)))
  print(ind)
  Xt<-X
  Xt<-max(Xt)
  num<-sum(ind*Xt)
  den<-sum(Xt)+eps
  C<-(num/den)
  S=sqrt(sum(((ind-C)^2)*Xt)/den)
  C=C/(fs/2.0)
  S=S/(fs/2.0)
  print(C)
  print(S)  
  return(C)
}




stEnergyEntropy<-function(X)
{
  numOfShortBlocks<-10
  Eol<-sum(X^2)   
  L<-length(X)
  subWinLength<-as.integer(floor(L/numOfShortBlocks))
  if(L!= (subWinLength * numOfShortBlocks))
  {
    X<- X[1: subWinLength * numOfShortBlocks + 1]
    
    
    
  }
  
  subWindows<-matrix(X, subWinLength, numOfShortBlocks, byrow = TRUE)
  s<-colSums (subWindows^2, na.rm = FALSE, dims = 1)/(Eol + eps)
  En<- (-sum(s * log2(s + eps)))
  return (En)
}

stSpectralFlux<-function(X,Xprev)
{
  eps = 0.00000001
  sumX=sum(X+eps)
  sumPrevX=sum(Xprev+eps)
  F=sum((X/sumX-Xprev/sumPrevX)^2)
  return (F)
}
stChormaFeaturesInit<-function(nfft,fs)
{
  k=seq(1,nfft)
  freqs=c()
  for (f in k)
  {
    freqs=append(freqs,(((f+1)*5)/(2*20)))
  }
  print(freqs)
  cp=27.50
  nChroma=round(12.0*log2(freqs/cp),digits=0)
  l<-NROW(nChroma)
  print(l)
  print(nChroma)
  nFreqsPerChroma=matrix(0,l,1)
  uChroma=unique(nChroma)
  for (u in uChroma)
  {  
    idx=which(nChroma==u,arr.ind = TRUE)
    nFreqsPerChroma[idx]=NCOL(idx[0])
    
  }
  nFreqsPerChroma=t(nFreqsPerChroma)
  mylist<-list(val1=nChroma,val2=nFreqsPerChroma)
  
  return(mylist)
}




