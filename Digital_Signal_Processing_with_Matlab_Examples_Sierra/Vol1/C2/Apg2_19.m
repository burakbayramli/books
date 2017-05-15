% Student´s PDF
v=-5:0.01:5; %values set
nu=3; %random variable parameter ("degrees of freedom")
ypdf=tpdf(v,nu); %Student´s PDF
plot(v,ypdf); %plots figure
axis([-5 5 0 0.4]);
xlabel('values'); title('Student´s PDF');
