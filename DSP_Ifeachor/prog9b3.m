
%
%  m-file to illustrate simple interpolation and 
%  decimation operations (Program 9B.2, p644).
%	File name: prog9b3.m
%	An Illustration of sampling rate changes using upfirdn by a factor of 4
%
Fs=1000;											%	sampling frequency
A=1.5;											%	relative amplitudes
B=1;
f1=50;											%	signal frequencies
f2=100;
t=0:1/Fs:1;										%	time vector
x=A*cos(2*pi*f1*t)+B*cos(2*pi*f2*t);	%	generate signal
y=resample(x,4,1);							%	interpolate signal by 4
stem(x(1:25))									%	plot original signal
xlabel('Discrete time, nT ')
ylabel('Input signal level')
figure
stem(y(1:100))									%	plot interpolated signal.
xlabel('Discrete time, 4 x nT')
ylabel('Interpolated output signal level')
y1=resample(y,1,4);
figure
stem(y1(1:25))									%	plot decimated signal.
xlabel('Discrete time, nT')
ylabel('Decimated output signal level')

