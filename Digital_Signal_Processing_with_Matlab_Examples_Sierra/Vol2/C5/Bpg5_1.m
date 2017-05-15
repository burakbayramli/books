% Frequency domain Wiener filtering
% sine signal + noise
fx=1; %signal frequency in Hz
wx=2*pi*fx; %signal frequency in rad/s

fs=60; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(3-tiv); %time intervals set

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

Y=fft(y); %Fourier transform of y
fly=real(ifft(Y.*WH)); %apply the Wiener filter

%display---------------------------

figure(1)
plot(t,y,'k'); %plots figure
axis([0 3 -3.5 3.5]);
xlabel('seconds'); title('sine+noise signal');

figure(2)
plot(t,fly,'k'); %plots figure
axis([0 3 -3.5 3.5]);
xlabel('seconds'); title('filtered signal');
