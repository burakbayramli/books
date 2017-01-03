function Firfilt

%FIR filtering program - program performs FIR filtering	using coefficients	 
%and data in user specified files. 		 

clear all;
data_file = fopen('filterin.dat','r');	%read the data to be filtered from file
x = fscanf(data_file,'%f'); 
coef_file = fopen('filtcoef.dat','r');	%read the filter coefficients from file
[b,n]=fscanf(coef_file,'%f',inf);
fclose('all');

y=filter(b,1,x);	%filtering


%Plot the results
bx=fft(x,512);
by=fft(y,512);
[h, w]=freqz(b,1,1024);
subplot(3,1,1),plot(1000*(0:255)/512,abs(bx(1:256))), ylabel('X(f)');  
subplot(3,1,2),plot(w/pi,abs(h)), ylabel('H(f)');
subplot(3,1,3),plot(1000*(0:255)/512,abs(by(1:256))), ylabel('Y(f)');