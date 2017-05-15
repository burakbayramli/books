% Weibull PDF
v=0:0.01:2.5; %values set
alfa=1; m=3; %random variable parameters
ypdf=weibpdf(v,alfa,m); %Weibull PDF
plot(v,ypdf); %plots figure
axis([0 2.5 0 1.2]);
xlabel('values'); title('Weibull PDF');
