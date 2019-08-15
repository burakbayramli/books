function sol = ch4ex4
r = 3.5;
m = 19;
options = ddeset('Events',@events,'InitialY',19.001,...
                 'RelTol',1e-4,'AbsTol',1e-7);
sol = dde23(@ddes,0.74,19,[0, 40],options,r,m);
%plot(sol.y,sol.yp);
plot(sol.y,sol.yp,'-k');
xlabel('y(t)');
ylabel('y''(t)');
%print -depsc ch4fig4a
n1 = find(sol.ie == 1);
x1 = sol.xe(n1);
y1 = sol.ye(1,n1);
n2 = find(sol.ie == 2);
x2 = sol.xe(n2);
y2 = sol.ye(1,n2);
figure
%plot(sol.x,sol.y,'k',x1,y1,'rs',x2,y2,'bo')
plot(sol.x,sol.y,'-k',x1,y1,'ks',x2,y2,'ko','MarkerSize',8)
xlabel('Time t');
ylabel('y(t)');
figure
plot(sol.x,sol.y,'-k',x1,y1,'ks',x2,y2,'k*','MarkerSize',8,...
     'MarkerFaceColor','k')
xlabel('Time t');
ylabel('y(t)');
figure
plot(sol.x,sol.y,'-k',x1,y1,'ks','MarkerSize',6,...
     'MarkerFaceColor','k')
hold 
plot(x2,y2,'ko','MarkerSize',6)%,'MarkerFaceColor','w')
xlabel('Time t');
ylabel('y(t)');
%print -depsc ch4fig4b
%=======================================================
function dydt = ddes(t,y,Z,r,m)
dydt = r*y*(1 - Z/m);

function [value,isterminal,direction] = events(t,y,Z,r,m)
dydt = r*y*(1 - Z/m);
value = [dydt; dydt];
direction = [+1; -1];
isterminal = [0; 0];