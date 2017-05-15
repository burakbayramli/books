% Rayleigh PDF
v=0:0.01:5; %values set
beta=1; %random variable parameter
ypdf=raylpdf(v,beta); %Rayleigh PDF
plot(v,ypdf); %plots figure
axis([0 5 0 0.7]);
xlabel('values'); title('Rayleigh PDF');
