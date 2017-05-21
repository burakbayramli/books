% Sum of sines signal 
fy=300; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s

fs=20; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(5-tiv); %time intervals set (5 seconds)

N = 100

y=0.6*sin(wy*t)+0.3*sin(3*wy*t)+0.2*sin(5*wy*t); %signal data set

fou=fft(y,fs);
hmag=real(fou); ah=hmag/N;
stem(0:9,ah(1:10)); hold on;
plot([0 10],[0 0],'k');
print ('out1.png')

pkg load signal
order = 32
fc1 = 4
fc2 = 0
fs = 20
filter_type = 'low'
window_type = 'hamming'
analyse_plot = 'n'
f1 = my_sinc_filter_low(order, fc1, fs);

fou=fft(f1,fs);
hmag=real(fou); ah=hmag/N;
stem(0:9,ah(1:10)); hold on;
plot([0 10],[0 0],'k');
print ('out2.png')
