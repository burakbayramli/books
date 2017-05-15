% Histogram of a random signal with normal PDF
fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(100-tiv); %time intervals set (10000 values)
N=length(t); %number of data points
y=randn(N,1); %random signal data set
v=-4:0.1:4; %value intervals set
hist(y,v); colormap(cool); %plots histogram
xlabel('values'); title('Histogram of random signal with normal PDF');
