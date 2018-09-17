library("seewave")
source(paste0("R",.Platform$file.sep,"audioIO.R"))


#frame will have to be converted into a vector

##Time-domain audio features


stZCR <- function(frame){
  count = length(frame)
  countZ = sum(abs(diff(sign(frame)))) / 2
  return (countZ / (count-1.0))
}


stEnergy<-function(frame)
{
  
  
  return (sum(frame^2)/length(frame))
  
}





stEnergyEntropy<-function(frame, numOfShortBlocks)
{
  eps = 0.00000001
  if (missing(numOfShortBlocks)){
    numOfShortBlocks<-10
  }
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
  ##(ind)
  Xt<-X
  Xt<-max(Xt)
  num<-sum(ind*Xt)
  den<-sum(Xt)+eps
  C<-(num/den)
  S=sqrt(sum(((ind-C)^2)*Xt)/den)
  C=C/(fs/2.0)
  S=S/(fs/2.0)
  ##print(C)
  ##print(S)  
  return(c(C, S))
}




stSpectralEntropy<-function(X, numOfShortBlocks)
{
  eps = 0.00000001
  if(missing(numOfShortBlocks)){
    numOfShortBlocks<-10
  }
  
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


stSpectralRollOff <- function(X, c, fs){
  eps = 0.00000001
  totalEnergy = sum(X ** 2) + eps
  fftlength = length(X)
  Thres = c*totalEnergy
  CumSum = cumsum(X ** 2) + eps
  a = which(CumSum > Thres)
  if(length(a) > 0){
    mC = a[1] / fftlength
  }
  else{
    mC = 0.0
  }
  return (mC)
}

stHarmonic<-function(frame, fs)
{
  eps = 0.00000001
  m = round(0.016*fs) -
    p<-c()
    a<-c()
    M<-round(0.016 * fs) - 1
    ##print(cat("M", M))
    R<-cor(frame, frame)
    ##print(cat("R", R))
    g<-R[length(frame)-1]
    ##print(cat("g", g))
    R<-rev(R)
    ##print(cat("R", R))
    a<-sign(R)
    ##print(cat("a", a))
    k=1
    m[1]=a[1]
    for (i in 2:length(a)){
      m[i]<-a[i]-a[i-1]
      
    }
    ##print(m)
    
    for (i in 1:length(m))
    {
      if(m[i]!=0)
      {
        p[k]=m[i]
        k<-k+1
      }
      
    }
    ##print(p)
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
    ##print(m0)
    Gamma[m0:M]<-R[m0:M] /(sqrt(g * Csum[M:m0]) + eps)
    ##print(cat("gamma", Gamma))
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
    ##print(cat("HR",HR))
    ##print(cat("f0", f0))
    return(c(f0, HR))
}
#arr<-array(c(-11,3,-55,6,60,80,-57,-316,-523,56,34,-819),dim=c(3,4))
##print(cat(arr))
#stHarmonic(arr , 10000)

#MFCC will be called from tineR

stChromaFeaturesInit<-function(nfft,fs)
{
  
  k=seq(1,nfft)
  freqs=c()
  for (f in k)
  {
    freqs=c(freqs, (((f+1)*fs)/(2*nfft)))
  }
  ##print(freqs)
  cp = 27.50
  nChroma = round(12.0*log2(freqs/cp),digits=0)
  l = length(nChroma)
  ##print(l)
  ##print(typeof(nChroma))
  nFreqsPerChroma = rep(0, l) 
  uChroma=unique(nChroma)
  for (u in uChroma)
  {  
    idx=which(nChroma==u,arr.ind = TRUE)
    nFreqsPerChroma[idx]=length(idx)
    
  }
  ##print(nFreqsPerChroma)
  return(list(nChroma, nFreqsPerChroma))
}

stChromaFeatures <- function(X, fs, nChroma, nFreqsPerChroma){
  
  chromaNames = c('A', 'A#', 'B', 'C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#')  
  spec = X ** 2
  ##print(X)
  ##print(nFreqsPerChroma)
  if (max(nChroma) < length(nChroma)){
    C = rep(0, length(nChroma))
    C[nChroma + 1] = spec
    C  = C / nFreqsPerChroma[nChroma + 1]
  }
  
  else{
    I = which(nChroma > length(nChroma))[1]
    C = rep(0, length(nChroma))
    C[nChroma[1:I - 1]] = spec
    C = C / nFreqsPerChroma
    
  }
  ##print(C[which(C!=0, arr.ind = TRUE)])
  finalC = matrix(rep(0, 12), nrow = 12, ncol = 1)
  ##print(finalC)
  
  newD = ceiling(length(C) / 12) * 12
  ##print(cat('newD', newD))
  C2 = rep(0, newD)
  C2[1:length(C)] = C
  
  C2 = matrix(C2, ncol = length(C2) / 12, nrow = 12)
  C2 = t(C2)
  ##print(length(colSums(C2)))
  finalC = matrix(colSums(C2), nrow = 12)
  ##print(dim(finalC))
  finalC = finalC / sum(spec)
  #stop("nigga")
  return(list(chromaNames, finalC))
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
      ##print("hello")
      chromagram=t(C)
    }else{
      chromagram=rbind(chromagram,t(C))
    }
    ##print(chromagram)
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
  ##print(chromagram)
  ##print(TimeAxis)
  ##print(FreqAxis)
  
  ret=list(TimeAxis=TimeAxis,chromagram=chromagram,FreqAxis=FreqAxis)
  return (ret)
}

#ans=list()
##ans=stChromagram(signal,Fs,Win,Step)
##print(ans$TimeAxis)
##print(ans$chromagram)
##print(ans$FreqAxis)


library(signal)
phormants<-function(x,Fs)
{ 
  w=c()
  Fs=10
  x=c(18,19,100,15,11,1)
  N1=length(x)
  N=seq(0,N1-1)
  i=1
  for (j in N)
  {
    ##print(j)
    tri=cos((2*3.14*j)/(N1-1))
    w[i]=0.54-0.46*tri
    i=i+1
  }
  x1=x*w
  x1=decimate(x1,1,1.0:0.63, ftype = "iir")
  ncoeff=2+Fs/1000
  
}

#Add spectogram and beatextraction

stFeatureExtraction <- function(signal, win, step){
  Fs = signal@samp.rate
  Win = as.integer(round(Fs * win))
  Step = as.integer(round(Fs * step))
  ##print(signal@left)
  #stop("signal testing")
  #Signal normalized at input stage
  ##print("st1")
  
  N = length(signal@left)
  curPos = 1
  countFrames = 0
  nFFT = Win / 2
  
  ##print("st2")
  stChromaFeaturesInitReturns = stChromaFeaturesInit(nFFT, Fs)
  ##print("st3")
  nChroma = stChromaFeaturesInitReturns[[1]]
  ##print(nChroma)
  nFreqsPerChroma = stChromaFeaturesInitReturns[[2]]
  ##print("st5")
  numOfTimeSpectralFeatures = 8
  numOfHarmonicFeatures = 0
  nceps = 12
  numOfChromaFeatures = 13
  totalNumOfFeatures = numOfTimeSpectralFeatures + nceps + numOfHarmonicFeatures + numOfChromaFeatures
  ##print(cat("st6", win, step))
  MFCC = melfcc(signal, wintime = win, hoptime = step)
  ##print("st7")
  stFeatures = c()
  ##print(cat("N", N))
  while (curPos + Win - 1 < N){
    countFrames = countFrames +  1
    ##print(cat("indices", curPos,"|", curPos + Win - 1, length(signal@left[curPos:(curPos + Win - 1)])))
    x = signal@left[curPos:(curPos + Win - 1)]
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
    curFV = c()
    ##print(cat("x", x))
    curFV[1] = zcr(x, f = Fs, win = NULL, plot = FALSE)                              # zero crossing rate
    ##print("1")
    curFV[2] = stEnergy(x)                           # short-term energy
    ##print("2")
    curFV[3] = stEnergyEntropy(x)                    # short-term entropy of energy
    ##print(3)
    stSpectralCentroidAndSpreadReturns = stSpectralCentroidAndSpread(X, Fs) # spectral centroid and spread
    ##print(3)
    curFV[4] = stSpectralCentroidAndSpreadReturns[[1]]  
    ##print(4)
    curFV[5] = stSpectralCentroidAndSpreadReturns[[2]]   
    ##print(5)
    curFV[6] = stSpectralEntropy(X)                  # spectral entropy
    ##print(6)
    curFV[7] = stSpectralFlux(X, Xprev)              # spectral flux
    ##print(7)
    curFV[8] = stSpectralRollOff(X, 0.90, Fs)        # spectral rolloff
    ##print(8)
    for (i in 9:20)
    {
      curFV[i] =  MFCC[countFrames,i - 8]   # MFCCs
    }
    ##print(cat(numOfTimeSpectralFeatures + 1, numOfTimeSpectralFeatures+nceps, MFCC[countFrames,]))
    stChromaFeaturesReturns = stChromaFeatures(X, Fs, nChroma, nFreqsPerChroma)
    chromaNames = stChromaFeaturesReturns[[1]]
    chromaF = stChromaFeaturesReturns[[2]]
    ##print(cat("chromaF", chromaF))
    ##print(cat(numOfTimeSpectralFeatures + nceps + 1 , numOfTimeSpectralFeatures + nceps + numOfChromaFeatures, length(chromaF)))
    for(i in 21:32){
      curFV[i] = chromaF[i-20]
    }
    curFV[numOfTimeSpectralFeatures + nceps + numOfChromaFeatures] = sd(chromaF)
    ##print(stFeatures)
    ##print("|")
    stFeatures = c(stFeatures, curFV)
    ##print(cat(countFrames, length(stFeatures)))
    ##print("st8")
    ##print()
    #stop("testing   curFV")
    rm(curFv)
    for(i in X){
      ##print('-')
      Xprev[length(Xprev) + 1] =  i
    }
  }
  
  #stFeatures = numpy.concatenate(stFeatures, 1) #Doubt
  ##print(stFeatures)
  stFeatures = t(matrix(stFeatures, nrow = 33))
  #stFeatures = sapply (stFeatures, function (x) {length (x) <- length(numOfTimeSpectralFeatures + nceps + numOfChromaFeatures); return (x)})
  ##print(length(stFeatures[1,]))
  #stop("testing stFeatures")
  return (stFeatures)
  
  
}

mtFeatureExtraction <- function(signal, fs, MtWin, MtStep, stWin, stStep) {
  
  mtWin = round(MtWin * fs)
  mtStep = round(MtStep * fs)
  Fs = signal@samp.rate
  mtWinRatio = as.integer(round(mtWin / round(stStep * fs)))
  ##print(cat(mtWinRatio, mtWin))
  mtStepRatio = as.integer(round(mtStep / round(stStep * fs)))
  
  mtFeatures = list()
  
  stFeatures = stFeatureExtraction(signal, stWin, stStep)
  ##print(stFeatures)
  ##print("yostFE")
  numOfFeatures = length(stFeatures[1,])
  #print(numOfFeatures)
  #stop("te te testing")
  numOfStatistics = 2
  
  
  ##print("mt1")
  
  for (i in 1:numOfFeatures){
    ##print("mt2")
    curPos = 1
    N = length(stFeatures[,i])
    ##print(cat("mt3", N))
    mt1 = c()
    mt2 = c()
    while(curPos < N){
      ##print("mt4")
      N1 = curPos
      N2 = curPos + mtWinRatio
      if (N2 > N1){
        N2 = N
      }
      ##print(cat("{}", N1, N2, i, dim(stFeatures)))
      curStFeatures = stFeatures[N1:N2,i]
      #stop("yaya")
      
      mt1 = c(mt1, mean(curStFeatures))
      mt2 = c(mt2, sd(curStFeatures))
      curPos = curPos + mtStepRatio
    }
    mtFeatures = c(mtFeatures, mt1, mt2)
  }
  mtFeatures = t(matrix(mtFeatures, nrow = numOfFeatures*numOfStatistics))
  #stFeatures = t(matrix(stFeatures, nrow = 33))
  #mtFeatures = sapply (mtFeatures, function (x) {length (x) <- length(curStFeatures); return (x)})
  ##print(mtFeatures)
  #stop("wewewe")
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
  ##print("czcaca")
  if (missing(computeBEAT)){
    computeBEAT = FALSE
  }
  allMtFeatures = c()
  processingTimes = c()
  
  types = list("*.wav", "*.mp3")
  wavFilesList = c()
  for (files in types){
    wavFilesList = c(wavFilesList, Sys.glob(file.path(dirName, files)))
  }
  if(length(wavFilesList) == 0){
    return(NULL)
  }
  ##print(wavFilesList)
  wavFilesList = sort(wavFilesList)
  wavFilesList2 = c()
  for (i in 1:length(wavFilesList)){
    cat("Analyzing file", i,"of", length(wavFilesList),":", encoded_text_to_latex(wavFilesList[i], encoding = "utf8"),"\n")
    if (file.size(wavFilesList[i]) == 0){
      print("\t(EMPTY FILE -- SKIPPING)\n")
      next
    }
    ##print(cat("wavfile", wavFilesList[i]))
    wfile = readAudioFile(wavFilesList[i])
    ##print("yo")
    t1 = Sys.time()
    Fs = wfile@samp.rate
    
    if(length(wfile@left) < (Fs / 10)){
      print("\tAUDIO FILE TOO SMALL - SKIPPING\n")
      next
    }
    wavFilesList2[length(wavFilesList2) + 1] =  wavFilesList[i]
    
    if (computeBEAT){
      #mtFeatureExtractionReturns = mtFeatureExtraction(x, Fs, round(mtWin * Fs), round(mtStep * Fs), round(Fs * stWin), round(Fs * stStep))
      #MidTermFeatures = mtFeatureExtractionReturns[1]
      #stFeatures = mtFeatureExtractionReturns[2]
      #beatExtractionReturns = beatExtraction(stFeatures, stStep)
      #beat = beatExtractionReturns[1]
      #beatConf = beatExtractionReturns[2]
    }
    else{
      mtFeatureExtractionReturns = mtFeatureExtraction(wfile, Fs, mtWin, mtStep, stWin, stStep)
      ##print("yomtFER")
      MidTermFeatures = mtFeatureExtractionReturns[[1]]
      ##print(dim(MidTermFeatures))
      
      
    }
    MidTermFeatures = t(MidTermFeatures)
    MidTermFeatures = matrix(as.integer(MidTermFeatures), nrow = dim(MidTermFeatures)[1])
    ##print((as.integer(MidTermFeatures)))
    MidTermFeatures = rowMeans(MidTermFeatures)
    ##print(length(MidTermFeatures))
    #stop("te tet testing")
    rowlength = length(MidTermFeatures)
    
    if(!(anyNA(MidTermFeatures)) && !(any(!is.finite(MidTermFeatures)))){
      if(computeBEAT){
        MidTermFeatures[length(MidTermFeatures) + 1] = beat
        MidTermFeatures[length(MidTermFeatures) + 1] = beatConf
      }
      if(length(allMtFeatures) == 0){
        allMtFeatures = c(allMtFeatures, MidTermFeatures)
      }
      else{
        allMtFeatures = c(allMtFeatures, MidTermFeatures)
      }
      t2 = Sys.time()
      duration = as.numeric(length(wfile)) / as.numeric(Fs)
      processingTimes[length(processingTimes) + 1] =  (as.POSIXct(t2) - as.POSIXct(t1)) / duration
      
    }
  }
  if (length(processingTimes) > 0){
    cat("Feature extraction complexity ratio:", mean(processingTimes), "x realtime\n")
  }
  allMtFeatures = t(matrix(allMtFeatures, nrow = rowlength))
  #print(dim(allMtFeatures))
  #stop("ye yippy yeye")
  return(list(allMtFeatures, wavFilesList2))
}

dirsWavFeatureExtraction <- function(dirNames, mtWin, mtStep, stWin, stStep, computeBEAT){
  if (missing(computeBEAT)){
    computeBEAT = FALSE
  }
  features = list()
  classNames = c()
  fileNames = c()
  shapeF = c()
  for(i in 1:length(dirNames)){
    d = dirNames[i]
    #print(cat("dirname", d))
    dirWavFeatureExtractionReturns = dirWavFeatureExtraction(d, mtWin, mtStep, stWin, stStep)
    f = dirWavFeatureExtractionReturns[[1]]
    shapeF = dim(f)
    fn = dirWavFeatureExtractionReturns[[2]]
    #if(dim(f) > 0){
      #features = c(features, f)
      features[[i]] = f
      fileNames = c(fileNames, fn)
      if (d[length(d)] == .Platform$file.sep){
        x = strsplit(d, .Platform$file.sep)
        classNames = c(classNames, x[[1]][length(x)])
      }
      else{
        x = strsplit(d, .Platform$file.sep)
        #print(x[[1]][length(x) + 1])
        classNames = c(classNames, x[[1]][length(x) + 1])
      }
    #}
    
  }
  #features = array(features, dim = c(length(dirNames), shapeF[2], shapeF[1]))
  #print(features[1,,1])
  #stop("boo")
  return(list(features, classNames, fileNames))
}
#shortTermWindow = 0.050
#shortTermStep = 0.050


#a = dirsWavFeatureExtraction(c("../temp1", "../temp2", "../temp3"), 1.0, 1.0, shortTermWindow, shortTermStep)
#traceback(dirsWavFeatureExtraction(c("../temp1", "../temp2"), 1.0, 1.0, shortTermWindow, shortTermStep))
#USE SPECTPROP TO ADD MORE PROPERTIES
