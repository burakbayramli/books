% polynomial fitting example
x=0:0.1:10; %independent variable data set
y=x.^2+3*x+15; %example of y=f(x)
N=length(x); %number of data points
di=randn(1,N); %random disturbance
ym=y+(10*di); %adding some disturbance to data

[P,S]=polyfit(x,ym,2); %polinomial fitting of disturbed data
ye=polyval(P,x); %estimated y values

%display of results
figure(1)
plot(x,y,':g'); hold on; %plots original data
plot(x,ym,'xr'); %plots disturbed data
plot(x,ye,'k'); %plots the fitting
title('polynomial data fitting, y(x) as dots, ye(x) solid'); 
xlabel('x'); ylabel('y');

cor=corrcoef(ym,ye); %fit quality checking

cor
P
S
