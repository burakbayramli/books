%plot_sinc
clear, clf
D=1; b1=-2; b2=2;
t=b1+[0:100]/100*(b2-b1);
bounds=[b1 b2];
%passing the parameter(s) through input argment
subplot(223), ez_plot1('sinc1',bounds,D)
axis([b1 b2 -0.4 1.2])
%passing the parameter(s) through VARARGIN
subplot(224), ez_plot('sinc1',bounds,D)
axis([b1 b2 -0.4 1.2])