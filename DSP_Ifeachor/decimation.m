function decimation
									
%program performs multi-stages of decimation on data in a user-specified data file	
%enough data must be guaranteed when using a large overall decimation fator

clear all;

r = [2 2 3 2];	% decimation factor array for different stages
FIR = 0;	% 1 - use FIR filter, 0 - use IIR filter
n = 0;	% order of IIR filter or FIR filter length
			% n=0 for 30 points FIR filter or 8th order Chebyshev type I LPF filter
in = fopen('decimation.dat','r') ;
[x,count] = fscanf(in,'%g',inf);	% data to be decimated
y = x;
fclose(in);
for i=1:length(r)
	if n==0	%decimating data use default filter
		if FIR
			y = decimate(y,r(i),'fir');	
		else
			y = decimate(y,r(i));
		end
	else
		if FIR
			y = decimate(y,r(i),n,'fir');	
		else
			y = decimate(y,r(i),n);
		end
	end
end
plot(1:count,x,1:prod(r):count,y(1:length(y)),'o');
