function cgdft								
%	function cgdft compute DFT coefficients using DIT FFT algorithm
%	function cgfft.m is used to implement the constant geometry FFT		

clear all;
direction = 1; 			%1	- forward DFT, -1 - inverse DFT 
in=fopen('datain.dat','r');
[x,count]=fscanf(in,'%g %g',[2 inf]);
fclose(in);
x = x(1,:)+x(2,:)*i;		% form complex numbers
npt = 2^(nextpow2(count/2));			% find the next power of two	
x(1:npt) = [x; zeros(npt-count/2,1)]; % x is padded with trailing zeros to npt

y=cgfft(x,npt,direction);	% calculate the constant geometry FFT

% Save/Print the results
out=fopen('dataout.dat','w');
fprintf(out,'%g %g\n',[real(y); imag(y)]);
fclose(out);
subplot(2,1,1),plot(1:npt,x); title('Input Signal'); 
subplot(2,1,2),plot(1:npt,y); title('Output Signal');