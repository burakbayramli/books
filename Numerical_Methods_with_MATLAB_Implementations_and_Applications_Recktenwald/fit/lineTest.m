function lineTest
% lineTest  Fit four data points to a line

x = [1 2 4 5];    y = [1 2 2 3];

c = linefit(x,y)
xfit = [0 6];  yfit = c(1)*xfit + c(2);
plot(x,y,'o',xfit,yfit,'-')
grid on;
xlabel('x values');  ylabel('y data and fit function');
