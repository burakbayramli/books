function Optimal
									 
clear all;
n = 34; f = [0 0.28 0.36 0.66 0.74 1]; m = [0 0 1 1 0 0]; w = [12.2 1 12.2];
ftype = 1;
if ftype==1	 
	b = remez(n,f,m,w)	%multiband filter
elseif ftype==2
	b = remez(n,f,m,w,'differentiator');	%differentiator
elseif ftype==3
   b = remez(n,f,m,w,'hilbert');	%Hilbert transform
else
   error('initialisation error');
end

%Plot the results
[h,w] = freqz(b,1,512); plot(f,m,w/pi,abs(h));
