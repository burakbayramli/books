%Obtain transfer function (TrF) from continuous frequency response
%TrF case 2
%no noise
% continuous time transfer function:
num2Ns=[10 0]; den2Ns=[1 3 10]; %G2(s)=10s/(s^2+3s+10);

wr=logspace(-1,2); %frequency values for response (rad/s)

%continuous frequency response of G2(s)
H2Ns=freqs(num2Ns,den2Ns,wr); %G2(s) frequency response

%using invfreqs to obtain the TrF
na=2; %denominator degree
nb=1; %numerator degree
[num2Es,den2Es]=invfreqs(H2Ns,wr,nb,na); %TrF computation

H2Es=freqs(num2Es,den2Es,wr); %^G2(s) frequency response

%comparing frequency responses
plot(H2Ns,'xr'); hold on;
plot(H2Es,'k');
x1=-1; x2=4; y1=-2; y2=2;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('complex frequency response, G2N(z)as x & G2E(z) solid');
xlabel('real'); ylabel('imaginary');

num2Ns
num2Es

den2Ns
den2Es
