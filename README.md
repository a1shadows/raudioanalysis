# raudioanalysis
Alternate to the pyaudioanalysis python package in R.
This R module is an alternaticve to the pyaudioanalysis library in python. It is still in the development stage. Up to this point, feature extraction has been implemented in R. Various fearures have been extracted taking into consideration the various types of categories that audio can be categorized into. 
The followings features were extracted:

● Zero Crossing Rate

● Energy

● Entropy of Energy

● Spectral Centroid

● Spectral Spread

● Spectral Entropy

● Spectral Flux

● Spectral Rolloff

● MFCCs

● Chroma Vector


We selected the features on the basis of information obtained from existing python packages like Pyaudioanalysis
and from academic literature that was available regarding audio classification.

When successive samples of the audio input have different algebraic signs then zero crossing rate occurs. Measure
of frequency content of a signal is known as the arte of zero crossings. It is the number of times the amplitude of the
audio input passes through the value of zero in a given frame of time, in simple words how frequently it changes its
sign from positive to negative or vice versa. Also analysis suggest that high crossing rate implies speech signal is
unvoiced and low crossing rate implies signal is voiced.

Entropy of energy is the entropy of sub-frames&#39; normalized energies. It can be interpreted as a measure of abrupt
changes. Speech signals appear more unordered as compared to music considering the observations of spectrograms
and signals.

Spectral Centroid is used to determine where does the centre of mass of the given spectrum lies. It is closely
related to the brightness of a sound. Mathematically it is computed by taking the weighted average of of all the
frequencies in the given signal which in turn are computed with the help of fourier transform by using the
magnitudes as weights. Sometimes spectral centroid is used in reference with the median of the input because both
of them are measures of central tendency. So at times both of them display similar behaviour in some of the
situations. Higher centroid values indicate much brighter textures having high rising frequencies. Centroid also gives
shape to sharpness of sound which is again related to high frequency of sound.

A technique used for transmitting telecommunication or radio signals is known as Spectral Spread. Higher the
value of spectral spread, more distributed the spectrum is on both sides if the centroid whereas lower values implies
that the spectrum is highly confined near the centroid.

Spectral Entropy is a quantitative analysis of spectral disorder of a given input audio. Mainly it is used to
determine voiced and silence regions of speech. It finds use in speech recognition because of its this discriminatory
property. Entropy can also be used to capture peakness and formants of a distribution.

The spectral flux is defined as the squared difference between the normalized magnitudes of successive spectral
distributions that correspond to successive signal frames. Flux is an important feature for the separation of music
from speech. This feature is specifically used to regulate the timbre of the audio signal.

Spectral Rolloff is referred to as the critical frequency below which eighty five percent of magnitude distribution of
the input is concentrated. Similar to that of spectral centroid it is a measure of spectral shape which yields values for
frequencies in high ranges. So it can be concluded that there is a strong correlation between spectral centroid and
spectral rolloff.

MFCCs are a compact representation of the spectrum of an audio signal taking into account the nonlinear human
perception of pitch, as described by the mel scale. The Short Time Fourier Transform (STFT) coefficients of each
frame are grouped into a set of 40 coefficients. MFCC is calculated using a set of 40 weighting curves that in turn
simulate the frequency perception of the human hearing system. Next step in the process is to take logarithm of the
coefficients. Post that decorrelation is done by doing discrete cosine transform (DCT). Normally the five first
coefficients are taken as features. Small musical units are detected the combination of chords and MFCCs.
Chroma vector refers to a set of twelve pitch classes represented as a vector. Distribution of energy is calculated for
every Chroma vector and as a result we get an updated audio signal of twelve dimensional Chroma distribution
vector.

The code is divided into various files based on their application. Up to this point, the feature extraction file is complete, while the classification file is in progress.
