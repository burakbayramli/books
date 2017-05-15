%Fourier Transform of pulse train signal
fy=1; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s
Ty=1/fy; %signal period in seconds

Ns=256; W=20;
fs=Ns*fy; %sampling frequency in Hz
y1=zeros(256,1); y1(1:W)=1; y1((256-W):256)=1; %signal period
fou=fft(y1); %Fourier Transform (set of complex numbers)
%get set of harmonic amplitudes:
ah(1)=fou(1)/Ns;
hmag=real(fou);
ah(2:Ns)=(hmag(2:Ns)*2)/Ns; 

stem(0:49,ah(1:50)); hold on; %plot of first 50 harmonics
plot([0 50],[0 0],'k');
xlabel('Hz'); title('pulse train signal harmonics');

