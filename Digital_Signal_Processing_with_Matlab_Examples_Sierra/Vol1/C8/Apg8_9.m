% Spectra of frequency modulated signals

% frequency modulation of sine signal
fa=40; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

fc=1000; %carrier frequency in Hz
wc=2*pi*fc; %carrier frequency in rad/s

fs=30000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.1-tiv); %time intervals set (0.1 seconds)
w1=-fs/2:-1; w2=1:fs/2; w=[w1 w2]; %vector of frequencies for spectra

beta=20; %modulation depth
y=cos((wc*t)+(beta*sin(wa*t))); %modulated signal data set

subplot(3,1,1)
beta=1; %modulation depth
y=cos((wc*t)+(beta*sin(wa*t))); %modulated signal data set
ffy=fft(y,fs); %Fourier transform of y(t)
sy=fftshift(real(ffy));sy=sy/max(sy);
plot(w,sy); %plots spectral density of y(t)
axis([-3000 3000 -1 1]);
ylabel('beta=1'); title('Y(w)');

subplot(3,1,2)
beta=5; %modulation depth
y=cos((wc*t)+(beta*sin(wa*t))); %modulated signal data set
ffy=fft(y,fs); %Fourier transform of y(t)
sy=fftshift(real(ffy));sy=sy/max(sy);
plot(w,sy); %plots spectral density of y(t)
axis([-3000 3000 -1 1]);
ylabel('beta=5');

subplot(3,1,3)
beta=10; %modulation depth
y=cos((wc*t)+(beta*sin(wa*t))); %modulated signal data set
ffy=fft(y,fs); %Fourier transform of y(t)
sy=fftshift(real(ffy));sy=sy/max(sy);
plot(w,sy); %plots spectral density of y(t)
axis([-3000 3000 -1 1]);
xlabel('Hz'); ylabel('beta=10');

