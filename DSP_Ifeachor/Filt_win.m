function Filt_win

%This program uses the window method to design FIR filters. 
%The following windows are supported: Rectangular, Hamming, Hanning, Blackman, 				 
%and Kaiser. 							 

% ftype:  []  	 for lowpass  filter
%	  'high' for highpass filter
%	  [] 	 for bandpass filter
%	  'stop' for bandstop filter
% wtype:  1 for Rectangular window
%	  2 for Hamming window
%	  3 for Hanning window
%	  4 for Blackman window
%	  5 for Kaiser window

clear all;
wtype = 2 ;
Fs = 8000; 	%sampling frequency
n = 52;		%the order of the filter
Wn = 0.4375;    %normalised cutoff frequency (2*Fstop/Fs)
beta = 0; 	%Kaiser window beta parameter
ftype=[];	
window=[];	%Hamming window will be used if variable window is empty

if wtype==1
	window=ones(1,n+1);	%Rectangular window
elseif wtype==3
	window=hanning(n+1);	%Hanning window
elseif wtype==4
	window=blackman(n+1);	%Blackman window
elseif wtype==5
	window=kaiser(n+1,beta);%Kaiser window
end
b = fir1(n,Wn,ftype,window)

% Plot the frequency response
freqz(b,1,1024,Fs);
