% ftdemo - Discrete Fourier transform demonstration program
clear all; help ftdemo; % Clear memory and print header

%* Initialize the sine wave time series to be transformed.
N = input('Enter the number of points: ');
freq = input('Enter frequency of the sine wave: ');
phase = input('Enter phase of the sine wave: ');
tau = 1;  % Time increment
t = (0:(N-1))*tau;              % t = [0, tau, 2*tau, ... ]
y = sin(2*pi*t*freq + phase);   % Sine wave time series
f = (0:(N-1))/(N*tau);          % f = [0, 1/(N*tau), ... ] 

%* Compute the transform using desired method: direct summation
%  or fast Fourier transform (FFT) algorithm.
flops(0);  % Reset the flops counter to zero
Method = menu('Compute transform by','Direct summation','FFT');
if( Method == 1 );             % Direct summation
  twoPiN = -2*pi*sqrt(-1)/N;
  for k=0:N-1
    expTerm = exp(twoPiN*(0:N-1)*k);
    yt(k+1) = sum(y .* expTerm);
  end
else                           % Fast Fourier transform
  yt = fft(y);
end
fprintf('Number of floating point operations = %g\n',flops);

%* Graph the time series and its transform.
figure(1); clf;  % Clear figure 1 window and bring forward
plot(t,y);
title('Original time series');
ylabel('Amplitude');  xlabel('Time');
figure(2); clf;  % Clear figure 2 window and bring forward
plot(f,real(yt),'-',f,imag(yt),'--');
legend('Real','Imaginary  ');
title('Fourier transform');
ylabel('Transform');  xlabel('Frequency');

%* Compute and graph the power spectrum of the time series.
figure(3); clf;  % Clear figure 3 window and bring forward
powspec = abs(yt).^2;
semilogy(f,powspec,'-');
title('Power spectrum (unnormalized)');
ylabel('Power');  xlabel('Frequency');
