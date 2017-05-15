% Transfer function + delay, for study: frequency response (complex plane)
%case 2d
Td=0.5; %pure delay in seconds
% continuous time transfer function:
num2Ns=[10 0]; den2Ns=[1 3 10]; %G2(s)=10s/(s^2+3s+10)

%frequency response
wr=logspace(0,2,200); %frequency values for response (rad/s)
H2Ns=freqs(num2Ns,den2Ns,wr); %G2(s) frequency response
H2Nsd=H2Ns.*exp(-j*Td*wr); %adding delay to G2(s)

%display frequency response
plot(H2Nsd,'k'); hold on;
x1=-3; x2=3; y1=-4; y2=2;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('G2(s)+delay complex frequency response');
xlabel('real'); ylabel('imaginary');
