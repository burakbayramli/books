% A beta PDF
v=0:0.01:1; %values set
alpha=7; beta=2; %random variable parameters
ypdf=betapdf(v,alpha,beta); %beta PDF
plot(v,ypdf,'k'); %plots figure
xlabel('values'); title('beta PDF');
