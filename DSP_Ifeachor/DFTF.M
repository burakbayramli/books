function DFTF								
%	Program to compute DFT coefficients using DIT FFT	
%  (Program 3c.2, p171; program name: dftf.m)
%	
clear all;
direction = -1;	%1 - forward DFT, -1 - inverse DFT
in = fopen('dataout.dat','r');
x = fscanf(in,'%g %g',[2,inf]);
fclose(in);
x = x(1,:)+x(2,:)*i;		% form complex numbers

if direction==1 
	y=fft(x,length(x))		% compute FFT
else
	y=ifft(x,length(x))		% compute IFFT
end

% Save/Print the results
out=fopen('dataout.dat','w');
fprintf(out,'%g %g\n',[real(y); imag(y)]);
fclose(out);
subplot(2,1,1),plot(1:length(x),x); title('Input Signal');
subplot(2,1,2),plot(1:length(y),y); title('Output Signal');