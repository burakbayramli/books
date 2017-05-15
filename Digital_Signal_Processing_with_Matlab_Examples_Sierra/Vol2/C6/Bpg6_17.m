% Transfer function + delay, for study: frequency response (complex plane)
%case 1d
Td=0.5; %pure delay in seconds
% continuous time transfer function:
num1Ns=10; den1Ns=[1 10]; %G1(s)=10/(s+10)

%frequency response
wr=logspace(0,2,200); %frequency values for response (rad/s)
H1Ns=freqs(num1Ns,den1Ns,wr); %G1(s) frequency response
H1Nsd=H1Ns.*exp(-j*Td*wr); %adding delay to G1(s)

%display frequency response
plot(H1Nsd,'k'); hold on;
x1=-1.2; x2=1.2; y1=-1.2; y2=1.2;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('G1(s)+delay complex frequency response');
xlabel('real'); ylabel('imaginary');
