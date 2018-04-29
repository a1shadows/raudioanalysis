library("tools")
library("tuneR")

readAudioFile <- function(filepath){
  #Returns the sampling rate and numeric values representing the sound clip
  extention = file_ext(filepath)
  
  if (extention == "mp3"){
    waveObj = readMP3(filepath)
  }
  
  else if(extention == "wav"){
    waveObj = readWave(filepath)
  }
  
  #audio file extracted. Applying Pre-processing.
  if (waveObj@stereo == TRUE){
    waveObj = mono(waveObj)
  }
  
  waveObj = noSilence(waveObj)
  waveObj = normalize(waveObj)
  
  #return(list(waveObj@samp.rate, waveObj@left))
  return(waveObj)
}
