%plot_sinc0
clear, clf
global D
D=1; b1=-2; b2=2;
t=b1+[0:100]/100*(b2-b1);
%passing the parameter(s) through global variables
x=sinc1(t,D);
subplot(231), plot(t,x)
axis([b1 b2 -0.4 1.2])

%passing the parameter(s) through arguments of the function
x=sinc2(t);
subplot(234), plot(t,x)
axis([b1 b2 -0.4 1.2])

bounds=[b1 b2];
subplot(232), ez_plot1('sinc1',bounds,D)
axis([b1 b2 -0.4 1.2])
subplot(235), ez_plot1('sinc2',bounds)
axis([b1 b2 -0.4 1.2])

subplot(233), ez_plot('sinc1',bounds,D)
axis([b1 b2 -0.4 1.2])
subplot(236), ez_plot('sinc2',bounds)
axis([b1 b2 -0.4 1.2])