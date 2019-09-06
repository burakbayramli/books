% Program 'rocket_obs_u.m' for generating the closed-loop
% gimbal angle input for a launch vehicle to circular orbit while
% guided by a normal acceleration controlled, bi-output
% reduced-order compensator based upon measurement and feedback of
% speed and altitude.
% (c) 2010 Ashish Tewari
% T: time vector (s); x: Runge-Kutta solution of 'rocket_obs.m'.
% x(:,1): radius vector (km); x(:,2): speed vector (km/s)
% x(:,3): flight-path angle vector (rad.); x(:,4): reduced-order
% observer state vector.
%
function u=rocket_obs_u(T,x)
global tn;
%Nominal time listed in the calling program as a row vector.
global rn;
%Nominal radius listed in the calling program as a row vector.
global vn;
%Nominal speed listed in the calling program as a row vector.
global phin; %Nominal flight-path angle listed in the calling
%program as a row vector.
global un;
%Nominal forward acceleration input listed in the calling
%program as a row vector.
mu=398600.4; %Earth's gravitational constant (km^3/s^2)
dtr=pi/180;
n=size(T,1)
for i=1:n
  t=T(i,1);
r=interp1(tn,rn,t);
v=interp1(tn,vn,t);
phi=interp1(tn,phin,t);
U=interp1(tn,un,t);
A=[0 sin(phi) v*cos(phi);
2*mu*sin(phi)/r^3
0 -mu*cos(phi)/r^2;
(-v/r^2+2*mu/(v*r^3))*cos(phi) (1/r+mu/(v^2*r^2))*cos(phi) ...
-(v/r-mu/(v*r^2))*sin(phi)];
k=[0.01 0.1 1.6];
a1=A(1,2);a2=A(1,3);a6=A(2,1);a7=A(2,3);a8=A(3,1);a9=A(3,2);a10=A(3,3);
F=-1;
L2=(a10-F)/a7;
G=F*[0 L2]+[a8 a9]-[L2*a6 0];
H=1/v;
dr=x(i,1)-r;
dv=x(i,2)-v;
dphi=x(i,3)-phi;
z=x(i,4);
xo2=L2*dv+z;
du=-k(1,2)*dv-k(1,1)*dr-k(1,3)*xo2;
mu1=asin(du/U)/dtr;
if abs(mu1)>15
du=abs(U)*sin(15*dtr)*sign(du);
end

if abs(mu1)<0.5
du=0;
end
u(i,1)=asin(du/U);
end
  
