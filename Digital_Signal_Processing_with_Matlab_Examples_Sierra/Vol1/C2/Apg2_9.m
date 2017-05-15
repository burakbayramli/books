% Histogram of a random signal with log-normal PDF
fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(100-tiv); %time intervals set (10000 values)
N=length(t); %number of data points
mu=0; sigma=1; %random signal parameters
y=lognrnd(mu,sigma,N,1); %random signal data set
v=0:0.1:12; %value intervals set
hist(y,v); colormap(cool); %plots histogram
axis([0 8 0 700]);
xlabel('values'); title('Histogram of random signal with log-normal PDF');
