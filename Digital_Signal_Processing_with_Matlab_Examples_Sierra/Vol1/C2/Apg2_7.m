% Random signal with log-normal PDF
fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(2-tiv); %time intervals set (200 values)
N=length(t); %number of data points
mu=0; sigma=1; %random signal parameters
y=lognrnd(mu,sigma,N,1); %random signal data set
plot(t,y,'-k'); %plots figure
axis([0 2 0 12]);
xlabel('seconds'); title('random signal with log-normal PDF');
