% Transfer function + delay, for study: frequency response (Bode diagram)
%case 1d
Td=0.5; %pure delay in seconds
% continuous time transfer function:
num1Ns=10; den1Ns=[1 10]; %G1(s)=10/(s+10)

%frequency response
wr=logspace(0,2,80); %frequency values for response (rad/s)
H1Ns=freqs(num1Ns,den1Ns,wr); %G1(s) frequency response
H1Nsd=H1Ns.*exp(-j*Td*wr); %adding delay to G1(s)
subplot(2,1,1)
semilogx(wr,abs(H1Ns),'xr'); hold on;
semilogx(wr,abs(H1Nsd),'k'); hold on;
title('G1(s) as x & G1(s)+delay as solid');
axis([1 100 0 1.2]);
subplot(2,1,2)
semilogx(wr,angle(H1Ns),'xr'); hold on;
semilogx(wr,unwrap(angle(H1Nsd)),'k'); hold on;
xlabel('rad/s')
axis([1 100 -60 5]);

