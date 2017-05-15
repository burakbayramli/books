% Display of complex Morlet wavelet
t=-7:0.01:7; 
w0=5;
cc=exp(-0.5*(w0^2));
aux=exp(-j*w0*t)-cc;
psi=aux.*exp(-0.5*t.^2);

%plots the wavelet
subplot(1,3,1)
plot(t,abs(psi),'k'); %magnitude
axis([-7 7 0 1.2]);
title('Morlet wavelet magnitude');

subplot(1,3,2)
plot(t,real(psi),'k'); %magnitude
axis([-7 7 -1 1.2]);
title('real part');

subplot(1,3,3)
plot(t,imag(psi),'k'); %magnitude
axis([-7 7 -1 1.2]);
title('imaginary part');

