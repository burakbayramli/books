%do_fft
%the spectra of a 2-tone signal
clear, clf
w1=1.5*pi; w2=3*pi; %two tones
N=32; n=[0:N-1]; T=0.1; t=n*T;
xan=sin(w1*t)+ 0.5*sin(w2*t);
k=0:N-1; w0=2*pi/T; w=k*w0; 
Xa=fft(xan);
discrepancy=norm(xan-real(ifft(Xa))) %x[n] can be reconstructed from IFFT{X(k)}?
figure(1)
subplot(421), stem(t,xan,'.')
axis([0 (N-1)*T -2 +2])
subplot(423), stem(k,abs(Xa),'.')
axis([0 N-1 0 30])
%upsampling
N=64; n=[0:N-1]; T=0.05; t=n*T;
xbn=sin(w1*t)+ 0.5*sin(w2*t);
k=0:N-1; w0=2*pi/T; w=k*w0; 
Xb=fft(xbn);
subplot(422), stem(t,xbn,'.')
axis([0 (N-1)*T -2 +2])
subplot(424), stem(k,abs(Xb),'.')
axis([0 N-1 0 30])
%zero-padding
N=64; n=[0:N-1]; T=0.1; t=n*T;
................