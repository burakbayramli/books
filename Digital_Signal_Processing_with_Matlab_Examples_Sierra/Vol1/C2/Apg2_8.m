% Log-normal PDF
v=-3:0.01:6; %values set
mu=0; sigma=1; %random variable parameters
ypdf=lognpdf(v,mu,sigma); %log-normal PDF
plot(v,ypdf); hold on; %plots figure
axis([0 6 0 0.7]);
xlabel('values'); title('log-normal PDF');
