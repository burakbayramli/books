function ch2ex1
[t,y] = ode45(@f,[0,1],0);
%plot(t,y);
plot(t,y,'-k');
%print -depsc ch2fig1
%==============================
function dydt = f(t,y)
dydt = y(1)^2 + t^2;
