% Chi-square PDF
v=-3:0.01:16; %values set
nu=3; %random variable parameter ("degrees of freedom")
ypdf=chi2pdf(v,nu); %chi-square PDF
plot(v,ypdf); %plots figure
axis([0 16 0 0.3]);
xlabel('values'); title('chi-square PDF');
