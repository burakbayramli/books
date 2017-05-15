% Amplitude modulation of sine signal and spectra
fa=80; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

fc=1500; %carrier frequency in Hz
wc=2*pi*fc; %carrier frequency in rad/s

fs=16*1024; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.03-tiv); %time intervals set (0.03 seconds)

a=square(wa*t); %modulating signal a(t) (square wave)
MD=0.4; %modulation depth
A=1+(MD*a); %amplitude
y=A.*sin(wc*t); %modulated signal data set

subplot(3,1,1)
ffa=fft(a,fs); %Fourier transform of a(t)
sa=fftshift(real(ffa));sa=sa/max(sa);
w1=-fs/2:-1; w2=1:fs/2; w=[w1 w2];
%w=(-63*80):80:(64*80);
plot(w,sa); %plots spectral density of a(t)
axis([-1500 1500 -1 1]);
xlabel('Hz'); title ('A(w)');

subplot(3,1,2);
plot(t,y,'k'); %plots modulated signal
axis([0 0.03 -1.5 1.5]);
xlabel('seconds'); title('y(t)');

subplot(3,1,3)
ffy=fft(y,fs); %Fourier transform of y(t)
sy=fftshift(real(ffy));sy=sy/max(sy);
plot(w,sy); %plots spectral density of y(t)
axis([-3000 3000 -1 1]);
xlabel('Hz'); title('Y(w)');
