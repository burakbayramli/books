function sol = ch4ex4
r = 3.5;
m = 19;
options = ddeset('Events',@events,'InitialY',19.001,...
                 'RelTol',1e-4,'AbsTol',1e-7);
sol = dde23(@ddes,0.74,19,[0, 40],options,r,m);
plot(sol.y,sol.yp);
xlabel('y(t)');
ylabel('y''(t)');
n1 = find(sol.ie == 1);
x1 = sol.xe(n1);
y1 = sol.ye(1,n1);
n2 = find(sol.ie == 2);
x2 = sol.xe(n2);
y2 = sol.ye(1,n2);
figure
plot(sol.x,sol.y,'k',x1,y1,'rs',x2,y2,'bo')
xlabel('Time t');
ylabel('y(t)');
%=======================================================
function dydt = ddes(t,y,Z,r,m)
dydt = r*y*(1 - Z/m);

function [value,isterminal,direction] = events(t,y,Z,r,m)
dydt = ddes(t,y,Z,r,m);
value = [dydt; dydt];
direction = [+1; -1];
isterminal = [0; 0];