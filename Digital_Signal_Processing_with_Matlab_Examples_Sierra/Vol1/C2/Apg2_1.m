% Random signal with uniform PDF
fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(2-tiv); %time intervals set (200 values)
N=length(t); %number of data points
y=rand(N,1); %random signal data set
plot(t,y,'-k'); %plots figure
axis([0 2 0 1.2]);
xlabel('seconds'); title('random signal with uniform PDF');
