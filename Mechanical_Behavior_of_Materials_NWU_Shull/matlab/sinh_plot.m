close all
set(0,'defaultaxeslinewidth',2);
set(0,'defaultaxesfontsize',16)
set(0,'defaultlinelinewidth',2)
set(0,'defaultfigurepaperposition',[0 0 7 5])
set(0,'defaultfigurepapersize',[7,5])
x=linspace(0,2.5,100);
plot(x, sinh(x),'-b')
hold on
plot(x, x, '--r')
plot(x,exp(x)/2,'.r')
xlabel('x')
ylabel('y')
legend('y=sinh(x)', 'y=x', 'y=exp(x)/2', 'location', 'best');
print(gcf, '../figures/sinhplot.svg', '-dsvg')