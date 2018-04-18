library("seewave")
source("R/audioIO.R")
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



stHarmonic<-function(frame, fs)
{
  m<-c()
  p<-c()
  a<-c()
  M<-round(0.016 * fs) - 1
  print(cat("M", M))
  R<-cor(frame, frame)
  print(cat("R", R))
  g<-R[length(frame)-1]
  print(cat("g", g))
  R<-rev(R)
  print(cat("R", R))
  a<-sign(R)
  print(cat("a", a))
  k=1
  m[1]=a[1]
  for (i in 2:length(a)){
    m[i]<-a[i]-a[i-1]
    
  }
  print(m)
  
  for (i in 1:length(m))
  {
    if(m[i]!=0)
    {
      p[k]=m[i]
      k<-k+1
    }
    
  }
  print(p)
  if( length(p) == 0)
  {
    m0 <- length(R)-1
    
  }
  else
  {
    m0 <- p[1]
  }
  
  if(  M > length(R))
  {
    M<-length(R) - 1
  }
  Gamma<-rep(0,M)
  Csum<-cumsum(frame^2)
  print(m0)
  Gamma[m0:M]<-R[m0:M] /(sqrt(g * Csum[M:m0]) + eps)
  print(cat("gamma", Gamma))
  ZCR<-stZCR(Gamma)
  if(ZCR>0.15)
  {
    HR= 0.0
    f0<-0.0
  }
  
  else
  {
    if( length(Gamma) == 0)
      
    {  HR = 1.0
    blag<-0.0
    Gamma<-rep(0,M)}
    else
    {
      HR<-max(Gamma)
      blag<-argmax(Gamma)
      
    }
  }
  
  f0<-fs / (blag + eps)
  if (f0 > 5000)
  {
    f0 = 0.0
  }
  if (HR < 0.1)
  {
    f0 = 0.0
  }
  print(cat("HR",HR))
  print(cat("f0", f0))
  #return(c(f0, HR))
}
arr<-array(c(-11,3,-55,6,60,80,-57,-316,-523,56,34,-819),dim=c(3,4))
#print(cat(arr))
stHarmonic(arr , 10000)


library("dtt")
stMFCC<-function(X, fbank, nceps)
{
  mspec<-log10((X %% t(fbank))+eps)
  ceps<-dct(mspec)[1:nceps]
  
  return(ceps)
}

f<-c(1,2,3,4)
p<-
  x<-3
print(stMFCC(x,f,p))



library(signal)
phormants<-function(x,Fs)
{ w=c()
Fs=10
x=c(18,19,100,15,11,1)
N1=length(x)
N=seq(0,N1-1)
i=1
for (j in N)
{
  print(j)
  tri=cos((2*3.14*j)/(N1-1))
  w[i]=0.54-0.46*tri
  i=i+1
}
x1=x*w
x1=decimate(x1,1,1.0:0.63, ftype = "iir")
ncoeff=2+Fs/1000

}


mfccInitFilterBanks<-function(fs,nfft)
{
  
  lowfreq=133.33
  linsc=200/3
  logsc=1.0711703
  numLinFiltTotal=13
  numLogFilt<-27
  
  if(fs<8000)
  {
    nlogfil<-5
  }
  
  nFiltTotal = numLinFiltTotal+numLogFilt
  
  freqs = rep(0,nFiltTotal+2)
  
  l=length(freqs)
  numLinFiltTotalu=numLinFiltTotal+1
  freqs[1:numLinFiltTotal]<-lowfreq+(seq(numLinFiltTotal)*linsc)
  
  print(numLinFiltTotalu:l)
  freqs[numLinFiltTotal:l]<-freqs[numLinFiltTotal]*logsc
  po=seq(1,numLogFilt+2)
  po
  freqs[numLinFiltTotalu:l]<-freqs[numLinFiltTotalu:l]^po
  m=l-1
  heights<-2.0/(freqs[2:l]-freqs[1:m])
  fbank<-matrix(0,nFiltTotal,nfft)
  m1=nfft-1
  nfreqs<-seq(m1)/(1.0*nfft)*fs
  nfreqs
  m2=nFiltTotal-1
  for (i in seq(1:m2))
  {
    lowTrFreq=freqs[i]
    cenTrFreq=freqs[i+1]
    highTrFreq=freqs[i+2]
  }
  
  k1=floor(lowTrFreq*(nfft/fs))+1
  k2=floor(cenTrFreq*(nfft/fs))
  k1
  k2
  #############################################################
  lid=seq(k1,k2)
  print(cat("lid", lid))
  lslope=heights[i]/(cenTrFreq-lowTrFreq)
  print(cat("lslope", lslope))
  rid=seq(floor(cenTrFreq*nfft/fs)+1,floor(highTrFreq*nfft/fs))
  print(cat("rid", rid))
  rslope=heights[i]/(highTrFreq-cenTrFreq)
  #print(cat("rslope", print(cat("rid", rid))print(cat("rid", rid))", rslope))
  fbank[i][lid]=lslope*(nfreqs[lid]-lowTrFreq)
  fbank[i][rid]=rslope*(highTrFreq-nfreqs[rid])
}
#print(fbank)
#print(freqs)


#fs=1
#nfft=5
#mfccInitFilterBanks(fs,nfft)


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

stChromagram<-function(signal,Fs,Win,Step,PLOT=FALSE)
{
  
  win=as.integer(Win)
  step=as.integer(Step)
  signal=c(as.double(signal))
  signal=signal/(2.0 ^ 15)
  dc=mean(signal)
  m=abs(signal)
  ma=max(m)
  signal=(signal-dc)/(ma-dc)
  n=length(signal)
  curpos=0
  countframes=0
  nfft=as.integer(win/2)
  chromagram=c()
  #returnno=list()
  #nChroma,nfreq
  #returno=stChromaFeaturesInit(nfft,Fs)
  while(curpos+win-1<n)
  {
    countframes=countframes+1
    k6=curpos+win
    x=signal[curpos:k6]
    curpos=curpos+step
    X=abs(fft(x,inverse=FALSE))
    X=X[0:nfft]
    X=X/length(X)
    #l=list()
    chromanames=array(c(0,50,100,150),dim=c(1,4))
    r1=c(19,188,12,10)
    r2=c(190,18,15,80)
    C=array(c(r1,r2),dim=c(2,4))
    #l=stChromaFeatures(X,Fs,nC)
    C=C[,1]
    C
    if(countframes==1){
      print("hello")
      chromagram=t(C)
    }else{
      chromagram=rbind(chromagram,t(C))
    }
    print(chromagram)
    FreqAxis=chromanames
    TimeAxis=c()
    ro=NROW(chromagram)
    ro=ro
    row=seq(0,ro)
    for (t in row)
    {
      TimeAxis[t+1]=((t*Step)/Fs)
      
    }
    
  }
  print(chromagram)
  print(TimeAxis)
  print(FreqAxis)
  
  ret=list(TimeAxis=TimeAxis,chromagram=chromagram,FreqAxis=FreqAxis)
  return (ret)
}

ans=list()
ans=stChromagram(signal,Fs,Win,Step)
print(ans$TimeAxis)
print(ans$chromagram)
print(ans$FreqAxis)


stFeatureExtraction <- function(signal, win, step){
  Fs = signal@samp.rate
  Win = as.integer(win)
  Step = as.integer(step)
  
  #Signal normalized at input stage
  
  N = len(signal@left)
  curPos = 0
  countFrames = 0
  nFFT = Win / 2
  
  mfccInitFilterBanksReturns = mfccInitFilterBanks(Fs, nFFT)
  fbank = mfccInitFilterBanksReturns[1]
  freqs = mfccInitFilterBanksReturns[2]
  
  stChromaFeaturesInitReturns = stChromaFeaturesInit(nFFT, Fs)
  nChroma = stChromaFeaturesInitReturns[1]
  nFreqsPerChroma = stChromaFeaturesInitReturns[2]
  
  numOfTimeSpectralFeatueres = 8
  numOfHarmonicFeatures = 0
  nceps = 13
  numOfChromaFeatures = 13
  totalNumOfFeatures = numOfTimeSpectralFeatures + nceps + numOfHarmonicFeatures + numOfChromaFeatures
  
  stFeatures = list()
  while (curPos + Win - 1 < N){
    countFrames = countFrames +  1
    x = signal@left[curPos:curPos + Win - 1]
    curPos = curPos + Step
    X = abs(fft(x))
    X = X[1:nFFT]
    X = X / length(X)
    if (countFrames == 1){
      Xprev = c()
      for(i in X){
        Xprev = c(Xprev, i)
      }
    }
    curFV = rep(0, totalNumOfFeatures)
    
    curFV[1] = stZCR(x)                              # zero crossing rate
    curFV[2] = stEnergy(x)                           # short-term energy
    curFV[3] = stEnergyEntropy(x)                    # short-term entropy of energy
    stSpectralCentroidAndSpreadReturns = stSpectralCentroidAndSpread(X, Fs) # spectral centroid and spread
    curFV[4] = stSpectralCentroidAndSpreadReturns[1]  
    curFV[5] = stSpectralCentroidAndSpreadReturns[2]   
    curFV[6] = stSpectralEntropy(X)                  # spectral entropy
    curFV[7] = stSpectralFlux(X, Xprev)              # spectral flux
    curFV[8] = stSpectralRollOff(X, 0.90, Fs)        # spectral rolloff
    curFV[numOfTimeSpectralFeatures + 1 : numOfTimeSpectralFeatures+nceps] = stMFCC(X, fbank, nceps).copy()    # MFCCs
    
    stChromaFeaturesReturns = stChormaFeatures(X, Fs, nChroma, nFreqsPerChroma)
    chromaNames = stChromaFeaturesReturns[1]
    chromaF = stChromaFeaturesReturns[2]
    curFV[numOfTimeSpectralFeatueres + nceps + 1 : numOfTimeSpectralFeatueres + nceps + numOfChromaFeatures] = chromaF
    curFV[numOfTimeSpectralFeatures + nceps + numOfChromaFeatures] = std(chromaF)
    stFeatures = c(stFeatures, curFV)
  
    for(i in X){
      Xprev = c(Xprev, i)
    }
  }
  
  #stFeatures = numpy.concatenate(stFeatures, 1) #Doubt
  return (stFeatures)
  
}

mtFeatureExtraction <- function(signal, mtWin, mtStep, stWin, stStep) {
  
  Fs = signal@samp.rate
  mtWinRatio = as.integer(round(mtWin / stStep))
  mtStepRatio = as.integer(round(mtStep / stStep))
  
  mtFeatures = list()
  
  stFeatures = stFeatureExtraction(signal, stWin, stStep)
  numOfFeatures = length(srFeatures)
  numOfStatistics = 2
  
  for (i in 1 : numOfStatistics * numOfFeatures){
    mtFeatures = list(mtFeatures, list())
    
  }
  
  for (i in 1:numOfFeatures){
    curPos = 1
    N = length(stFeatures[i])
    while(curpos < N){
      N1 = curPos
      N2 = curPos + mtWinRatio
      if (N2 > N1){
        N2 = N
      }
      curStFeatures = stFeatures[i][N1:N2 + 1]
      
      mtFeatures[i] = list(mtFeatures[i], mean(curStFeatures))
      mtFeatures[i + numOfFeatures] = list(mtFeatures[i + numOfFeatures], sd(curStFeatures))
      curPos = curPos + mtStepRatio
    }
  }
  return (list(mtFeatures, stFeatures))
}

stFeatureSpeed <- function(signal, Win, Step){
  
  Fs = signal@samp.rate
  
  N = length(signal)
  curPos = 1
  countFrames = 0
  
  lowfreq = 133.33
  linsc = 200/3.
  logsc = 1.0711703
  nlinfil = 13
  nlogfil = 27
  nceps = 13
  nfil = nlinfil + nlogfil
  nfft = Win / 2
  if(Fs < 8000){
    nlogfil = 5
    nfil = nlinfil + nlogfil
    nfft = Win / 2
  }
  mfccInitFilterBanksReturns = mfccInitFilterBanks(Fs, nfft, lowfreq, linsc, logsc, nlinfil, nlogfil)
  fbank = mfccInitFilterBanksReturns[1]
  freqs = mfccInitFilterBanksReturns[2]
  
  numOfTimeSpectralFeatures = 8
  numOfHarmonicFeatures = 1
  totalNumOfFeatures = numOfTimeSpectralFeatures + nceps + numOfHarmonicFeatures
  stFeatures = list()
  
  while (curPos + Win <= N){
    countFrames = countFrames + 1
    x = signal@left[curPos : curPos + Win]
    curPos = curPos + Step
    X = abs(fft(x))
    X = X[1 : nfft]
    X = X / length(X)
    Ex = 0.0
    E1 = 0.0
    X[1 : 4] = 0
    stFeatures = list(stFeatures, stHarmonic(x, Fs))
  }
    
  return (stFeatures)
}

dirWavFeatureExtraction <- function(dirName, mtWin, mtStep, stWin, stStep, computeBEAT){
  if (missing(computeBEAT)){
    computeBEAT = FALSE
  }
  allMtFeatures = list()
  processingTimes = list()
  
  types = list("*.wav", "*.mp3")
  wavFilesList = list()
  for (files in types){
    wavFilesList = list(wavfile, Sys.glob(file.path(dirName, files)))
  }
  
  wavFilesList = sort(wavFilesList)
  wavFilesList2 = list()
  for (i in 1:length(wavFilesList)){
    print(cat("Analyzing file {0:", i,"} of {1:", length(wavFilesList),"}: {2:", encoded_text_to_latex(wavFilesList[i], encoding = "utf8"),"}"))
    if (file.size(wavFilesList[i]) == 0){
      print("\t(EMPTY FILE -- SKIPPING)")
      next
    }
    wfile = readAudioFile(wavFilesList[i])
    t1 = Sys.time()
    Fs = wfile@samp.rate
    
    if(length(wfile@left) < (Fs / 10)){
      print("\tAUDIO FILE TOO SMALL - SKIPPING")
      next
    }
    wavFilesList2 = list(wavFilesList2, wavFilesList[i])
    
    if (computeBEAT){
      mtFeatureExtractionReturns = mtFeatureExtraction(x, Fs, round(mtWin * Fs), round(mtStep * Fs), round(Fs * stWin), round(Fs * stStep))
      MidTermFeatures = mtFeatureExtractionReturns[1]
      stFeatures = mtFeatureExtractionReturns[2]
      beatExtractionReturns = beatExtraction(stFeatures, stStep)
      beat = beatExtractionReturns[1]
      beatConf = beatExtractionReturns[2]
    }
    else{
      mtFeatureExtractionReturns = mtFeatureExtraction(x, Fs, round(mtWin * Fs), round(mtStep * Fs), round(Fs * stWin), round(Fs * stStep))
      MidTermFeatures = mtFeatureExtractionReturns[1]
    
    }
    MidTermFeatures = t(MidTermFeatures)
    MidTermFeatures = rowMeans(MidTermFeatures)
    
    if(anyNA(MidTermFeatures) && any(!is.finite(MidTermFeatures))){
      if(computeBEAT){
        MidTermFeatures = list(MidTermFeatures, beat)
        MidTermFeatures = list(MidTermFeatures, beatConf)
      }
      if(length(allMtFeatures) == 0){
        allMtFeatures = MidTermFeatures
      }
      else{
        allMtFeatures[[length(allMtFeatures)+1]] = MidTermFeatures
      }
      t2 = Sys.time
      duration = as.float(length(x)) / as.float(Fs)
      processingTimes = list(processingTimes, (t2 - t1) / duration)
      
    }
  }
  if (length(processingTimes) > 0){
    print(cat("Feature extraction complexity ratio:", mean(processingTimes), "x realtime"))
  }
  return(list(allMtFeatures, wavFilesList2))
}

dirsWavFeatureExtraction <- function(dirNames, mtWin, mtStep, stWin, stStep, computeBEAT){
  if (missing(computeBEAT)){
    computeBEAT = FALSE
  }
  features = list()
  classNames = list()
  fileNames = list()
  for(i in 1:length(dirNames)){
    d = dirNames[i]
    dirWavFeatureExtractionReturns = dirWavFeatureExtraction(d, mtWin, mtStep, stWin, stStep, computeBEAT = computeBEAT)
    f = dirWavFeatureExtractionReturns[1]
    fn = dirWavFeatureExtractionReturns[2]
    if(dim(f) > 0){
      features = list(features, f)
      fileNames = list(fileNames, fn)
      if (d[length(d)] == .Platform$file.sep){
        x = strsplit(d, .Platform$file.sep)
        classNames = list(classNames, x[length(x) - 1])
      }
      else{
        x = strsplit(d, .Platform$file.sep)
        classNames = list(classNames, x[length(x)])
      }
    }
    
  }
  return(list(features, classNames, fileNames))
}