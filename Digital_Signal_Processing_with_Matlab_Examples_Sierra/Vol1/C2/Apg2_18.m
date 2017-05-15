% beta PDF
v=0:0.01:8; %values set
alfa=5; beta=3; %random variable parameters
ypdf=betapdf(v,alfa,beta); %beta PDF
plot(v,ypdf); %plots figure
axis([0 1 0 2.5]);
xlabel('values'); title('beta PDF');
