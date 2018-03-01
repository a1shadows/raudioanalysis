library("tools")
library("tuneR")

readAudioFile <- function(filepath){
  extention = file_ext(filepath)

  if (extention == "mp3"){
    waveObj = readMP3(filepath)
  }

  else if(extention == "wav"){
    waveObj = readWave(filepath)
  }

  if (waveObj@stereo == TRUE){
  waveObj = mono(waveObj)
  }
  return(waveObj@left)
}
