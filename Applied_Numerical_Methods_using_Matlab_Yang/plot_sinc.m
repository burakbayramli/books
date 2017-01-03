%plot_sinc
clear, clf
global D
D=1; b1=-2; b2=2;
t=b1+[0:100]/100*(b2-b1);
%passing the parameter(s) through global variables
subplot(221), plot(t,sinc1(t,D))
axis([b1 b2 -0.4 1.2])
%passing the parameter(s) through arguments of the function
subplot(222), plot(t,sinc1(t))
axis([b1 b2 -0.4 1.2])

