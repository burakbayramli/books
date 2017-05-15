% Histogram of a random signal with uniform PDF
fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(100-tiv); %time intervals set (10000 values)
N=length(t); %number of data points
y=rand(N,1); %random signal data set
v=0:0.02:1; %value intervals set
hist(y,v); colormap(cool); %plots histogram
xlabel('values'); title('Histogram of random signal with uniform PDF');
