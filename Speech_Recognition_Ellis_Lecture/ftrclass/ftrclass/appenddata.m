function[DO,LO] = appenddata(F,DI,LI,K)
% [DO,LO] = appenddata(F,DI,LI)   Add timit data to a training set
%      DI is a set of feature rows, and LI is a corresponding row 
%      of target labels.  F is the stem of a TIMIT file; the 
%      wavfile [F,'.wav'] will be read and converted to features,
%      and appended to DI to give DO.
%      The phone label file [F, '.phn'] will be read, mapped 
%      to labels using key array K, then appended to LI to give LO.
% 2001-03-28 dpwe@ee.columbia.edu

[d,sr] = wavread([F,'.wav']);
[t,l] = labreadlab([F,'.phn'],K);

framerate = 100;

% Put your own feature calculation line HERE
[cp,fr,fb,fc,fd] = mfcc(d,sr,framerate);
% Use the MFCCs as features
ftrs = cp';
% .. or we could use downsampled spectral features with the following line:
%ftrs = fc(1:4:end)';


% MFCC uses a 256 pt (16ms) window and skips the last frame. 
% Hence the window center times are:
tt=.008:.01:(length(d)/sr-.018);

% Get the corresponding labels
ll=labsamplabs(tt,t,l);

% Append to input arrays
DO = [DI;ftrs];
LO = [LI,ll];
