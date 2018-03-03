
eps = 0.00000001

stZCR <- function(frame){
  #takes tuneR wave object as input
  return(zcr(frame, wl = NULL))
}


stEnergy <- function(frame){
  #takes tuneR wave object as input| aditya
  return(sum(frame@left ** 2)/length(frame@left))
}

stEnergyEntropy<-function(frame, numOfShortBlocks){
  #takes tuneR wave object as input
  if(missing(numOfShortBlocks)){
    numOfShortBlocks = 10
  }

  Eol <- sum(frame@left ** 2)
  L<-length(frame@left)
  subWinLength<-as.integer(floor(L/numOfShortBlocks))

  if(L!= (subWinLength * numOfShortBlocks))
  {
    #frame<- filter(frame, between(row_number(), 0, subWinLength * numOfShortBlocks()))
    frame<- frame[1: subWinLength * numOfShortBlocks + 1]
    #error in slicing , filter not working
  }

  subWindows<-matrix(frame@left, subWinLength, numOfShortBlocks, byrow = TRUE)
  s<-colSums (subWindows ** 2, na.rm = FALSE, dims = 1)/(Eol + eps)
  Entropy<- (-sum(s * log2(s + eps)))
  return (Entropy)
}
