% Gamma-type PDFs
v=0:0.01:10; %values set
alfa=1; beta=1; %random variable parameters
ypdf=gampdf(v,alfa,beta); %gamma-type PDF
plot(v,ypdf,'k'); hold on; %plots figure
axis([0 10 0 1.2]);

alfa=2; beta=1; %random variable parameters
ypdf=gampdf(v,alfa,beta); %gamma-type PDF
plot(v,ypdf,'--k'); hold on; %plots figure

alfa=4; beta=1; %random variable parameters
ypdf=gampdf(v,alfa,beta); %gamma-type PDF
plot(v,ypdf,':k'); hold on; %plots figure

xlabel('values'); title('gamma-type PDFs');
