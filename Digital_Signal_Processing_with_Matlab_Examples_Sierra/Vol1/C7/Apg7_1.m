% GMP signal and spectrum 
fy=100; %signal central frequency in Hz
bw=0.2; %signal relative bandwidth
fs=1000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=-(0.2-tiv):tiv:(0.2-tiv); %time intervals set (0.4 seconds)

subplot(2,1,1)
y=gauspuls(t,fy,bw); %signal data set
plot(t,y,'k'); %plots figure
axis([-0.2 0.2 -1.2 1.2]);
xlabel('seconds'); title('Gaussian-modulated sine signal');


subplot(2,1,2)
Y=fft(y)/(fs/2); %Fourier transform of the signal
xf=0:1:(fs/2);
plot(xf(1:80),real(Y(1:80)),'k'); %plots spectrum
axis([0 80 -0.05 0.05]);
xlabel('Hz'); title('signal spectrum');
