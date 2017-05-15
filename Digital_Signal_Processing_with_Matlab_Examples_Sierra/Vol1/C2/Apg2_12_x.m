% The buried sine
fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(2-tiv); %time intervals set (200 values)
N=length(t); %number of data points
yr=randn(N,1); %random signal data set
ys=sin(15*2*pi*t); %sinusoidal signal (15 Hz)
y=ys+yr'; %the signal+noise
plot(t,ys,'k'); %plots buried sine
axis([0 2 -3 3]);
xlabel('seconds'); title('the buried sine');
