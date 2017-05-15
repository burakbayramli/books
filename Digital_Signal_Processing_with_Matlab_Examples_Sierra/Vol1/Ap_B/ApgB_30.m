% Pulse modulations of sine signal

% the pulses (bits):
a=[0 1 0 1 1 0 1 0 0 1]; %the modulating signal (bits)

fa=40; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

fc=6*fa; %carrier frequency in Hz
wc=2*pi*fc; %carrier frequency in rad/s

fs=10*fc; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;

%sampling the modulating signal
nsa=fs/fa; %number of samples per bit
Nb=length(a); %number of bits in a
nsmsg=Nb*nsa; %number of samples in the complete message

tmsg=nsmsg*tiv; %time for the message
t=0:tiv:(tmsg-tiv); %time intervals set

%time for a bit
tb=1/fa;
t1=0:tiv:(tb-tiv);
%carrier signal for 1 bit time
c1=sin(2*pi*fc*t1);

%modulating signal samples
us1=ones(1,nsa); %a vector of nsa ones
as=a(1)*us1;
for nn=2:Nb,
   as=cat(2,as,a(nn)*us1);
end

subplot(4,1,1)
plot(t,as,'k'); %plots the modulating signal
ylabel('bits message'); title('digital modulation of sine signal');
axis([0 0.25 -0.2 1.2]);

%ASK modulation
%modulated signal samples
ASKy=a(1)*c1; %carrier signal for first bit
for nn=2:Nb,
   ASKy=cat(2,ASKy,a(nn)*c1);
end

subplot(4,1,2)
plot(t,ASKy,'k');
axis([0 0.25 -1.2 1.2]);
ylabel('ASK');

%FSK modulation
fc1=4*fa; fc2=8*fa; %two fcarrier frequencies
c1=sin(2*pi*fc1*t1); %carrier for bit=0
c2=sin(2*pi*fc2*t1); %carrier for bit=1

%modulated signal samples
if a(1)==0, FSKy=c1; else FSKy=c2; end; %carrier signal for the first bit
for nn=2:Nb,
   if a(nn)==0, 
      FSKy=cat(2,FSKy,c1);
   else 
      FSKy=cat(2,FSKy,c2);
   end;
end

subplot(4,1,3)
plot(t,FSKy,'k');
axis([0 0.25 -1.2 1.2]);
ylabel('FSK');

%PSK modulation
pc1=0; pc2=pi; %two fcarrier phases
c1=sin((2*pi*fc*t1)+pc1); %carrier for bit=0
c2=sin((2*pi*fc*t1)+pc2); %carrier for bit=1

%modulated signal samples
if a(1)==0, PSKy=c1; else PSKy=c2; end; %carrier signal for the first bit
for nn=2:Nb,
   if a(nn)==0, 
      PSKy=cat(2,PSKy,c1);
   else 
      PSKy=cat(2,PSKy,c2);
   end;
end

subplot(4,1,4)
plot(t,PSKy,'k');
axis([0 0.25 -1.2 1.2]);
ylabel('PSK'); xlabel('seconds');

