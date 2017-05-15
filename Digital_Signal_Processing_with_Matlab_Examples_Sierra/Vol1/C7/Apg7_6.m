% 2 GMPs signal
fy1=40; %signal 1 central frequency in Hz
fy2=80; %signal 2 central frequency
bw=0.2; %signal relative bandwidth
fs=300; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
tp=-(0.2-tiv):tiv:(0.2-tiv); %time intervals set (0.4 seconds)
Np=length(tp); 
y1=gauspuls(tp,fy1,bw); %signal 1 data set
y2=gauspuls(tp,fy2,bw); %signal 2 data set
t=0:tiv:1; %complete time set (1 second);
Ny=length(t);
yn=zeros(1,Ny-(2*Np)); %intermediate signal
y=[y1 yn y2];
plot(t,y,'k'); %plots figure
%axis([0 1.2 -1.2 1.2]);
xlabel('seconds'); title('2 GMPs signal');
%
%print signal energy (Parseval)
disp('signal energy:')
Pyt=tiv*sum((abs(y)).^2) %time domain computation
c=fft(y,fs)/fs;
PYW=sum(abs(c).^2) %frequency domain computation