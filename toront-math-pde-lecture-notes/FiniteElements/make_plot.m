
x = 0:.1:1;
figure(1)
clf
plot(x,zeros(size(x)),'LineWidth',2);
hold on
plot(zeros(size(x)),x,'LineWidth',2);
plot(x,1-x,'LineWidth',2);
figure(1)



x = -3:.1:-2;
y = x+3;
plot(x,y,'LineWidth',2);
x = -4:.1:-2;
y = -1/2*(x+2)+1;
plot(x,y,'LineWidth',2);
m = (0-2)/(-3-(-4));
x = -4:.1:-3;
y = m*(x+3);
plot(x,y,'LineWidth',2);


axis([-4.3,1.3,-.3,2.3])
figure(1)
print triangle.ps