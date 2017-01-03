%nm1p26 : CtFT and ICtFT
clear, clf
global B D
%CtFT of A Rectangular Pulse Function
t=[-50:50]/10; %time vector
w=[-60:60]/10*pi; %frequency vector
D=1; %Duration of a rectangular pulse rD(t)
for k=1:length(w), Xw(k)=CtFT1('rDt',D*5,w(k)); end
subplot(221), plot(t,rDt(t))
subplot(222), plot(w,abs(Xw))
%ICtFT of A Sinc Spectrum
B=2*pi; %Bandwidth of a sinc spectrum sncB(w)
for n=1:length(t), xt(n)=ICtFT1('sincBw',B*5,t(n)); end
subplot(223), plot(t,real(xt))
subplot(224), plot(w,sincBw(w))
