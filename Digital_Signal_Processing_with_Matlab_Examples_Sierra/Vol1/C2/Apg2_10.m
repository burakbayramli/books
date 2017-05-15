% A skewed PDF with mean, median and mode
v=0:0.01:8; %values set
alfa=2;; %random variable parameter
ypdf=raylpdf(v,alfa); %Weibull PDF
plot(v,ypdf); hold on; %plots figure
axis([0 8 0 0.4]);
xlabel('values'); title('a skewed PDF');

fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(20-tiv); %time intervals set (2000 values)
N=length(t); %number of data points
y=raylrnd(alfa,N,1); %random signal data set
mu=mean(y); %mean of y
vo=median(y); %median of y
[pky,pki]=max(ypdf); %peak of the PDF
plot([mu mu],[0 0.33],'--k'); %mean
plot([vo vo],[0 0.37],':k'); %median
plot([v(pki) v(pki)],[0 pky],'-.k'); %mode
