library("seewave")
eps = 0.00000001

#frame is a tuneR wave class object

##Time-domain audio features


stZCR <- function(frame){
  return(zcr(frame, wl = NULL))
}


stEnergy <- function(frame){
  return(sum(frame@left ** 2)/length(frame@left))
}


stEnergyEntropy<-function(frame, numOfShortBlocks){
  if(missing(numOfShortBlocks)){
    numOfShortBlocks = 10
  }

  Eol <- sum(frame@left ** 2)
  L<-length(frame@left)
  subWinLength<-as.integer(floor(L/numOfShortBlocks))

  if(L!= (subWinLength * numOfShortBlocks))
  {
    frame<- frame[1: subWinLength * numOfShortBlocks + 1]
  }

  subWindows<-matrix(frame@left, subWinLength, numOfShortBlocks, byrow = TRUE)
  s<-colSums (subWindows ** 2, na.rm = FALSE, dims = 1)/(Eol + eps)
  Entropy<- (-sum(s * log2(s + eps)))
  return (Entropy)
}


##Frequency-domain audio features

stSpectralCentroidAndSpread<-function(X1)
{
  X = spec(X1)
  fs = X1@samp.rate
  print(X)
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
  return(c(C, S))
}


stEnergyEntropy<-function(X1, numOfShortBlocks)
{
  if (missing(numOfShortBlocks)){
    numOfShortBlocks<-10
  }

  Eol = sum(X ** 2)
  L = length(X)
  subWinLength = as.integer(floor(L/numOfShortBlocks))
  if(L != (subWinLength * numOfShortBlocks))
  {
    X =  X[1: subWinLength * numOfShortBlocks + 1]
  }

  subWindows = matrix(X, subWinLength, numOfShortBlocks, byrow = TRUE)
  s = colSums(subWindows ** 2, na.rm = FALSE, dims = 1) / (Eol + eps)
  En =  (-sum(s * log2(s + eps)))
  return (En)
}

