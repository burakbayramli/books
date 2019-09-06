% Program for specifying governing state equations
% of 2PBVP for flight with modulated fwd and normal
% acceleration inputs (to be called by 'bvp4c.m')
function dydx=normballisticode(x,y)
global R1;
global R2;
mu=398600.4;
dydx=[y(3)*cos(y(4))/y(2)
y(3)*sin(y(4))
-0.5*y(7)/R1-mu*sin(y(4))/y(2)^2
-0.5*y(8)/(R2*y(3))+(y(3)- ...
mu/(y(2)*y(3)))*cos(y(4))/y(2)
0
y(5)*y(3)*cos(y(4))/y(2)^2- ...
2*mu*sin(y(4))*y(7)/y(2)^3- ...
(2*mu/(y(2)*y(3))-y(3))*y(8)*cos(y(4))/y(2)^2
-y(5)*cos(y(4))/y(2)-y(6)*sin(y(4))- ...
y(8)*(0.5*y(8)/(R2*y(3)^3)+ ...
(1+mu/(y(3)^2*y(2)))*cos(y(4))/y(2))
y(5)*y(3)*sin(y(4))/y(2)-y(6)*y(3)*cos(y(4))+ ...
mu*cos(y(4))*y(7)/y(2)^2+(y(3)/y(2)- ...
mu/(y(2)^2*y(3)))*sin(y(4))*y(8)];
% Program for specifying boundary conditions for the 2PBVP.
% (To be called by 'bvp4c.m')
function res=normballisticbc(ya,yb)
mu=398600.4;
r0=6378.14;
dtr=pi/180;
res=[ya(1)
ya(2)-r0
ya(3)-0.001
ya(8)
yb(1)-30*dtr
yb(2)-r0-200
yb(3)-9
yb(4)-5*dtr];
