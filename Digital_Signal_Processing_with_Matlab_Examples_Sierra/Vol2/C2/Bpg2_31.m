% Display of complex B-spline wavelet
t=-9:0.01:9; 
m=2; wc=8; wb=5;
cc=sqrt(wb/(2*pi*m));
aa=(wb*t)/(2*m); aux=sin(aa)./aa;
psi=cc*(aux.^m).*exp(j*wc*t);

%plots the wavelet
subplot(1,3,1)
plot(t,abs(psi),'k'); %magnitude
axis([-9 9 0 0.8]);
title('complex B-spline wavlt. magn.');

subplot(1,3,2)
plot(t,real(psi),'k'); %magnitude
axis([-9 9 -0.8 0.8]);
title('real part');

subplot(1,3,3)
plot(t,imag(psi),'k'); %magnitude
axis([-9 9 -0.8 0.8]);
title('imaginary part');

