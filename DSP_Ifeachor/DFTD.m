function DFTD

clear all;
% Program to compute DFT coefficients directly
% (Program 3c.1, p170; program name: dftd.m)
%

direction = -1;	%1 - forward DFT, -1 - inverse DFT
in = fopen('datain.dat','r');
x = fscanf(in,'%g %g',[2,inf]);
fclose(in);
x = x(1,:)+x(2,:)*i;		% form complex numbers

if direction==1 
	y = x*dftmtx(length(x)) ;	%compute DFT
else
	y = x*conj(dftmtx(length(x)))/length(x);	%compute IDFT
end

% Save/Print the results
out=fopen('dataout.dat','w');
fprintf(out,'%g %g\n',[real(y); imag(y)]);
fclose(out);
subplot(2,1,1),plot(1:length(x),x); title('Input Signal');
subplot(2,1,2),plot(1:length(y),y); title('Output Signal');