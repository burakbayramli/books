% Simple Allpass filter example
a=0.4;
w=0:(2*pi/511):pi;
Anum=[1 -a];
Aden=[-a 1];
G=freqz(Anum,Aden,w);

%display
figure(1)
subplot(2,1,1)
plot(w,abs(G),'k'); grid;
axis([0 pi 0 1.5]);
title('frequency response of simple Allpass example');
xlabel('w');
subplot(2,1,2)
plot(w,angle(G),'k'); grid;
axis([0 pi 0 4]);
title('phase');
xlabel('w'); ylabel('rad')
