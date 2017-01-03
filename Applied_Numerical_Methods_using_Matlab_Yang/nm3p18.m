%nm3p18: windowing effect on DFT spectrum
w1=1.5*pi; w2=3*pi; %two tones
N=64; n=1:N; T=0.1; t=(n-1)*T; 
k=1:N; w0=2*pi/T; w=(k-1)*w0;
xbn=sin(w1*t)+ 0.5*sin(w2*t);
xbwn=windowing(xbn,'bt');
Xb=fft(xbn); Xbw=fft(xbwn);
subplot(421), stem(t,xbn,'.')
subplot(423), stem(k,abs(Xb),'.')
..............
