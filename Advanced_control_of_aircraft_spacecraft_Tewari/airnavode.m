% Program for specifying governing ODEs for optimal navigation
% expressed as state equations for the 2PBVP
%(to be called by 'bvp4c.m')
% (c) 2010 Ashish Tewari
function dydx=airnavode(x,y)
global vprime;
global vw;
global Aw;
global r;
global R;
global Omega;
v=sqrt(vprime^2-vw^2*sin(Aw-y(3))^2)+vw*cos(Aw-y(3));
u=-0.5*y(6)/(R*v);
dvdA=sin(Aw-y(3))*(1-cos(Aw-y(3))/sqrt(vprime^2-vw^2*sin(Aw-y(3))^2));
dydx=[v*cos(y(3))/r
v*sin(y(3))/(r*cos(y(1)))
v*sin(y(3))*tan(y(1))/r+u/v+Omega^2*r*sin(y(3))*sin(2*y(1))/(2*v)+ ...
2*Omega*sin(y(1))
v*sin(y(3))*tan(y(1))*sec(y(1))*y(5)/r+(v*sin(y(3))*sec(y(1))^2/r+ ...
Omega^2*r*sin(y(3))*cos(2*y(1))/v+2*Omega*cos(y(1)))*y(6)
0
(dvdA*cos(y(3))-v*sin(y(3)))*y(4)/r + ...
(v*cos(y(3))+dvdA*sin(y(3)))*y(5)/(r*cos(y(1))) + ...
(tan(y(1))*(v*cos(y(3))+dvdA*sin(y(3)))/r-u*dvdA/v^2+ ...
 0.5*Omega^2*r*sin(2*y(1))*(cos(y(3))/v-dvdA*sin(y(3))/v^2))*y(6)];

% Program for specifying boundary conditions for 2PBVP of
% optimal aircraft navigation.
% (To be called by bvp4c.m)
function res=airnavbc(ya,yb)
global lam0;
global lamf;
global del0;
global delf;
global A0;
res=[ya(1)-del0
ya(2)-lam0
ya(3)-A0
yb(1)-delf
yb(2)-lamf
yb(6)];
