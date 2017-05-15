% A message via 4-PSK

% the pulses (bits):
a=[0 1 0 0 1 0 1 1 0 1]; %the modulating signal (bits)
Nb=length(a); %number of message bits

fc=1000; %carrier frequency in Hz
ns=20; %number of samples per carrier cycle (also per bit-pair)
fs=ns*fc; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:((1/fc)-tiv); %time intervals set, 1 sine period (1 bit-pair)
tmsg=0:tiv:((Nb/(2*fc))-tiv); %time intervals set for the complete message

%carrier signals for bit-pair time
c1=sin(2*pi*fc*t);
c2=sin((2*pi*fc*t)+(pi/2));
c3=sin((2*pi*fc*t)+(pi));
c4=sin((2*pi*fc*t)+(3*pi/2));

subplot(2,1,1) 
xx=Nb/(10*fc); %for vertical lines
us1=ones(1,ns/2); %vector of ns/2 ones
as=a(1)*us1;
for nn=2:Nb,
   as=cat(2,as,a(nn)*us1);
end
plot(tmsg,as,'k'); hold on; %plots modulating signal
for nn=1:4,
  plot([nn*xx nn*xx],[-0.2 1.2],':b');
end
axis([0 Nb/(2*fc) -0.2 1.2]);
ylabel('message'); xlabel('seconds');
title('4-PSK modulation');

subplot(2,1,2)
cs=c2; %first two bits of a: (0 1)
cs=cat(2,cs,c1); %append 2nd two bits of a: (0 0)
cs=cat(2,cs,c3); %append 3rd two bits of a: (1 0)
cs=cat(2,cs,c4); %append 4th two bits of a: (1 1)
cs=cat(2,cs,c2); %append 5th two bits of a: (0 1)
plot(tmsg,cs,'k'); hold on; %plots modulated signal
for nn=1:4,
  plot([nn*xx nn*xx],[-1.2 1.2],':b');
end
axis([0 Nb/(2*fc) -1.2 1.2]);
ylabel('4-PSK signal'); xlabel('seconds');
