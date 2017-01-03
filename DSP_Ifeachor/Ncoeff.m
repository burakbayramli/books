function ncoeff
								
%program for estimating the number of coefficients of Parks_McClellan optimal FIR filters

clear all;
Fs = 8000;	
f = [1200 1500 2800 3000];	
m = [0 1 0];	
dev = [0.122 0.01 0.122];	
[n,f0,m0,w]=remezord(f,m,dev,Fs);	

%Output the filter order estimated and ckeck if it meets the given specifications
fprintf(1,'The filter order estimated is:  %d\n',n);
b=remez(n,f0,m0,w);
freqz(b,1,1024,Fs);
