% Wiener filtering
% sine signal + noise
% Details: bandwidth and filter coeffs.
fx=1; %signal frequency in Hz
wx=2*pi*fx; %signal frequency in rad/s

fs=60; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(4-tiv); %time intervals set

x=sin(wx*t); %signal data set
Nx=length(x);
v=randn(1,Nx); %normal noise
y=x+v; %sine+noise

%Wiener computations
X=fft(x); %Fourier transform of x
Sxx=abs(X).^2; %Sxx
V=fft(v); %Fourier transform of v
Svv=abs(V).^2; %Svv
WH=Sxx./(Sxx+Svv); %Fourier transform of the Wiener filter
wh=real(ifft(WH)); %filter impulse response

fly=filter(wh,1,y); %apply the Wiener filter

%display---------------------------

figure(1)
fiv=60/Nx;
fq=0:fiv:3;
plot(fq,WH(1:length(fq)),'k'); %plots figure
axis([fiv 3 -0.5 1.5]);
xlabel('Hz'); title('Frequency response of the filter');

figure(2)
plot(wh,'k'); hold on; %plots figure
limh=2/Nx;
plot([1+Nx/2 1+Nx/2],[-limh limh],'r--');
axis([0 Nx -limh limh]);
title('Filter coefficients');

figure(3)
plot(t,fly,'k'); %plots figure
axis([0 4 -3.5 3.5]);
xlabel('seconds'); title('filtered signal');
