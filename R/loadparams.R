library(readtext)
loadparameters<- function(){
  cat( readLines( "data.txt" ) , sep = "\n" )
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
  #quit()
}