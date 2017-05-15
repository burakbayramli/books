% Demodulation of a DSB signal
fa=80; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

fc=2000; %carrier frequency in Hz
wc=2*pi*fc; %carrier frequency in rad/s

fs=30000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.03-tiv); %time intervals set (0.03 seconds)

%the DSB signal
MD=0.4; %modulation depth
A=1+(MD*sin(wa*t)); %amplitude
y=A.*sin(wc*t); %modulated signal data set

%demodulation
N=length(t); d1=zeros(1,N);
%diode simulation
for tt=1:N,
   if y(tt)<0 
      d1(tt)=0;
   else
      d1(tt)=y(tt);
   end;
end;  
%the R-C filter
R=1000; C=0.000003;
fil=tf([R],[R*C 1]); %transfer function

d2=lsim(fil,d1,t); %response of the filter

subplot(2,1,1)
plot(t,d1,'k'); %plots diode output
xlabel('seconds'); title('rectified modulated signal');

subplot(2,1,2)
plot(t,d2,'k'); %plots filter output
xlabel('seconds'); title('demodulated signal')

