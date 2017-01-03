function IIRBZT

%Design program for IIR filters with Butterworth, Chebyshev, Inverse 		
%Chebyshev or Elliptic Characteristic using bilinear transformation	
%output arguments:
%n - estimated minimun filter order. 
%Wn - cutoff frequencies. 
%b,a - coefficients of digital filter
%ftype - 'low' 		for lowpass filter
%			'high' 		for highpass filter
%			'bandpass' 	for bandpass filter
%			'stop'  		for bandstop filter
%ctype - 1 for Butterworth filter
%			2 for Chebyshev filter
%			3 for Inverse Chebyshev filter
%			4 for Elliptic filter

%steps of designing a digital filter implied in the called functions:
% step 1: estimate the minimum order of the filter from specifications
% step 2: get analog, pre-warped frequencies
% step 3: convert to low-pass prototype estimate
% step 4: Get n-th order analog lowpass prototype with desired filter characters
% step 5: Transform to lowpass, bandpass, highpass, or bandstop of desired Wn
% step 6: Use Bilinear transformation to find discrete equivalent:

clear all; format;

Fs = 100000;			%sampling frequency(Hz). 
Wp = [20500 23500]/(Fs/2);	%passband edge frequency normalised by Fs/2. 
Ws = [19000 25000]/(Fs/2);	%stopband edge frewquency normalised by Fs/2.
Rp = 0.25;				%passband attenuation in dB. 
Rs = 45;				%stopband attenuation in dB
ctype = 4;			%character of filter
ftype = 'bandpass';		%type of filter

if ctype==1	
	[n,Wn]=buttord(Wp,Ws,Rp,Rs); 				 
	[b,a]=butter(n,Wn,ftype);		 
elseif ctype==2	
	[n,Wn]=cheb1ord(Wp,Ws,Rp,Rs); 			 
	[b,a]=cheby1(n,Rp,Wn,ftype);	
elseif ctype==3	
	[n,Wn]=cheb2ord(Wp,Ws,Rp,Rs); 			 
	[b,a]=cheby2(n,Rs,Wn,ftype);	
elseif ctype==4
	[n,Wn]=ellipord(Wp,Ws,Rp,Rs); 
	[b,a]=ellip(n,Rp,Rs,Wn,ftype);	
end

%Output the result
disp('Numerator coefficients (in descending powers of z):'); disp(b);
disp('Denominator coefficients (in descending powers of z):'); disp(a);
freqz(b,a,1024,Fs);
