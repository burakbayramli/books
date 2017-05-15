%Fourier Transfom of sawtooth signal 
fy=1; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s
Ty=1/fy; %signal period in seconds

Ns=256; %number of samples per signal period
fs=Ns*fy; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(Ty-tiv); %time intervals set (1 period)
y=sawtooth(wy*t); %signal data set

fou=fft(y); %Fourier Transform (set of complex numbers)
%get set of harmonic amplitudes:
bh(1)=fou(1)/Ns;
hmag=-imag(fou);
bh(2:Ns)=(hmag(2:Ns)*2)/Ns; 

stem(0:9,bh(1:10)); %plot of first 10 harmonics
xlabel('Hz'); title('sawtooth signal harmonics');

