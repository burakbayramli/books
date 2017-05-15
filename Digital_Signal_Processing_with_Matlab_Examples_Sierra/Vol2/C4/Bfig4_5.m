% Derivative of 1D Gaussian
% and its Fourier transform
x=-6:0.1:6;
N=length(x);

z=zeros(N,1); %space for the function
for nx=1:N,
   z(nx)=-x(nx)*exp(-0.5*x(nx)^2); 
end;

FZ=abs(fftshift(fft(z)));
FZ=FZ/max(FZ);
w=-pi:(2*pi)/(N-1):pi;

figure(1)
subplot(2,1,1)
plot(x,z,'k');
axis([-6 6 -0.8 0.8]);
title('derivative of Gaussian (dg)');
subplot(2,1,2)
r=50:72; %zoom on w range around 0
plot(w(r),FZ(r),'k');
axis([-0.6 0.6 0 1.1]);
title('|FFT(dg)|');
