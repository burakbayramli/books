t=0:0.05:10;
y=exp(-t).*(cos(sqrt(3)*t)+(1/sqrt(3))*sin(sqrt(3)*t));
plot(t,y);
title('Response due to initial conditions');
xlabel(' time t');
ylabel(' The output y(t)');
axis([0 10 -1 1]);
