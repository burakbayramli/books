% Program for specifying governing ODEs of great circle navigation
% expressed as state equations
% (c) 2010 Ashish Tewari
function dydx=airnav(x,y)
global vprime;
global vw;
global Aw;
global r;
global Omega;
v=sqrt(vprime^2-vw^2*sin(Aw-y(3))^2)+vw*cos(Aw-y(3));
dydx=[v*cos(y(3))/r
v*sin(y(3))/(r*cos(y(1)))
v*sin(y(3))*tan(y(1))/r];
