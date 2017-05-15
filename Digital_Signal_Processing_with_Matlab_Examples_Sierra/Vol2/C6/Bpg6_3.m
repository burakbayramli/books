% fitting a sinusoid
t=0:0.02:(2*pi); %independent variable data set
w=5; %parameter
y=sin(w*t); %example of y=sin(wt)
N=length(t); %number of data points
di=randn(1,N); %random disturbance
ym=y+(0.3*di); %adding some disturbance to data

ferror=inline('sum(abs(ym-sin(t*x)))'); %function to be minimised by x
[wex ferrorx]=fminbnd(ferror,1,7,[],t,ym); %find x for minimum error
ye=sin(wex*t); %estimated data

%display of results
figure(1)
plot(t,y,':g'); hold on; %plots original data
plot(t,ym,'xr'); %plots disturbed data
plot(t,ye,'k'); %plots the fitting
title('fitting of sinusoidal data, y(t) as dots, ye(t) solid'); 
xlabel('t'); ylabel('y');

cor=corrcoef(ym,ye); %fit quality checking

wex
ferrorx
cor