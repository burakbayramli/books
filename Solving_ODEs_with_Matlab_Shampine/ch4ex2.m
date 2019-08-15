function sol = ch4ex2
sol = dde23(@ddes,2,0.5,[0, 100]);
t = linspace(2,100,1000);
y = deval(sol,t);
ylag = deval(sol,t - 2);
plot(y,ylag)
xlabel('y(t)');
ylabel('y(t - 2)');
%=================================
function dydt = ddes(t,y,Z)
dydt = 2*Z/(1 + Z^9.65) - y;