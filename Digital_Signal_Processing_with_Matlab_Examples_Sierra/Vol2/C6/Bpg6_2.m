% fitting exponential data
x=1:0.02:5; %independent variable data set
y=exp(x.^2+5*x+3); %example of y=exp(f(x))
N=length(x); %number of data points
di=randn(1,N); %random disturbance
ym=y.*exp(3*di); %adding some disturbance to data

lym=log(ym); %get polynomial version

[P,S]=polyfit(x,lym,2); %polinomial fitting
lye=polyval(P,x); %estimated f(x) values
ye=exp(lye); %estimated data

%display of results
figure(1)
semilogy(x,y,':g'); hold on; %plots original data
semilogy(x,ym,'xr'); %plots disturbed data
semilogy(x,ye,'k'); %plots the fitting
title('polynomial fitting of exponential data, y(x) as dots, ye(x) solid'); 
xlabel('x'); ylabel('log y');

cor=corrcoef(ym,ye); %fit quality checking

cor
P
S
