% Approximation of delay by transfer function
Td=0.5; %pure delay in seconds
wr=logspace(0,2,200); %frequency values for response (rad/s)
Hdly=exp(-j*Td*wr); %frequency response corresponding to Td

%using invfreqs to obtain a TrF approximation
na=19; %denominator degree
nb=19; %numerator degree
[num,den]=invfreqs(Hdly,wr,nb,na); %TrF computation

Htrf=freqs(num,den,wr); %TrF frequency response

%compare phase of frequency responses cooresponding to the delay and to TrF
figure(1)
plot(wr,unwrap(angle(Hdly)),'xr'); hold on;
plot(wr,unwrap(angle(Htrf)),'k');
x1=-1.2; x2=1.2; y1=-1.2; y2=1.2;
title('phase of frequency responses corresponding to delay and TrF');
xlabel('rad/s');

%compare frequency responses cooresponding to the delay and to TrF
figure(2)
plot(Hdly,'xr'); hold on;
plot(Htrf,'k');
x1=-1.2; x2=1.2; y1=-1.2; y2=1.2;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('frequency responses corresponding to delay and TrF');
xlabel('real'); ylabel('imaginary');

%pole-zero map of TrF
figure(3)
gz=roots(num); gp=roots(den);
plot(gz,'ok'); hold on; %zeros plot
plot(gp,'xk'); %poles plot
x1=-40; x2=40; y1=-120; y2=120;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('zeros and poles of TrF');
xlabel('real'); ylabel('imaginary');

num1Ns
num1Es

den1Ns
den1Es
