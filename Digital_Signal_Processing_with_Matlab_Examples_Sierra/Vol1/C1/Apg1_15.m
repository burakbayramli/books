%Fourier Transfom of triangular signal 
fy=1; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s
Ty=1/fy; %signal period in seconds

Ns=256; %%number of samples per signal period
fs=Ns*fy; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(Ty-tiv); %time intervals set (1 period)
y=-sawtooth(wy*t,0.5); %signal data set

fou=fft(y); %Fourier Transform (set of complex numbers)
%get set of harmonic amplitudes:
ah(1)=fou(1)/Ns;
hmag=real(fou);
ah(2:Ns)=(hmag(2:Ns)*2)/Ns; 

stem(0:9,ah(1:10)); hold on; %plot of first 10 harmonics
plot([0 10],[0 0],'k');
xlabel('Hz'); title('triangular signal harmonics');



