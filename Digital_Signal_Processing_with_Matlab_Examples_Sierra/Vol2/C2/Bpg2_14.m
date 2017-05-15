% Display of Shannon scaling function and wavelet
t=-10:0.01:10; %time vector
phi=sinc(t) ; %the scaling function
psi=(2*sinc(2*t))-sinc(t); %the wavelet
figure(1)
subplot(2,1,1)
plot(t,phi,'k');
axis([-10 10 -0.4 1.2]);
xlabel('t')
title('Shannon scaling function');
subplot(2,1,2)
plot(t,psi,'k');
axis([-10 10 -1.2 1.2]);
xlabel('t')
title('Shannon wavelet');
