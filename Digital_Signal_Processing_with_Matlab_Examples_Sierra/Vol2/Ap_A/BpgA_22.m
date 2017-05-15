% Transfer function + delay, for study: frequency response (Bode diagram)
%case 2d
Td=0.5; %pure delay in seconds
% continuous time transfer function:
num2Ns=[10 0]; den2Ns=[1 3 10]; %G2(s)=10S/(s^2+3S+10)

%frequency response
wr=logspace(0,2,80); %frequency values for response (rad/s)
H2Ns=freqs(num2Ns,den2Ns,wr); %G2(s) frequency response
H2Nsd=H2Ns.*exp(-j*Td*wr); %adding delay to G2(s)
subplot(2,1,1)
semilogx(wr,abs(H2Ns),'xr'); hold on;
semilogx(wr,abs(H2Nsd),'k'); hold on;
title('G2(s) as x & G2(s)+delay as solid');
axis([1 100 0 4]);
subplot(2,1,2)
semilogx(wr,angle(H2Ns),'xr'); hold on;
semilogx(wr,unwrap(angle(H2Nsd)),'k'); hold on;
xlabel('rad/s')
axis([1 100 -60 5]);

