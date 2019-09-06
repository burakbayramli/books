%Calling program for simulating the great circle trajectory for
%navigation by fourth-order Runge-Kutta solution.
%Requires airnav.m.
%(c) 2010 Ashish Tewari
%y(1,1)= Latitude (rad.)
%y(2,1)= Longitude (rad.)
%y(3,1)= Velocity azimuth (rad.)
global del0; del0=51.5*pi/180; %initial latitude
global lam0;lam0=0; %initial longitude
global delf; delf=40.4*pi/180; %final latitude
global lamf;lamf=-73.5*pi/180; %final longitude
global vw;vw=20; %wind speed
global Aw;Aw=45*pi/180; %wind velocity azimuth
global vprime; vprime=270; %airspeed (m/s)
global r; r=6378.14e3+11000; %radius (m)
global Omega; Omega=7.2921e-5; %Earths rotational rate (rad/s)
tf=6.12*3600; %terminal time
dtr=pi/180;
% Computation of the initial velocity azimuth, A_0:
r0=[cos(del0)*cos(lam0) cos(del0)*sin(lam0) sin(del0)]
rf=[cos(delf)*cos(lamf) cos(delf)*sin(lamf) sin(delf)]
n=cross(r0,rf)/norm(cross(r0,rf))
cosi=dot([0 0 1],n)
sinA0=cosi/cos(del0);
A0=asin(sinA0)
v0=sin(A0)*[-sin(lam0) cos(lam0) 0]+ ...
cos(A0)*[-sin(del0)*cos(lam0) ...
-sin(del0)*sin(lam0) cos(del0)];
h0=cross(r0,v0)/norm(cross(r0,v0))
if abs(dot(h0,n))<=0.99
A0=pi-A0
end
% 4th order, 5th stage Runge-Kutta integration:
[x,y]=ode45(@airnav,[0 tf],[del0 lam0 A0]');
% Ground speed (m/s) follows:
v=sqrt(vprime^2-vw^2*sin(Aw-y(:,3)).^2)+vw*cos(Aw-y(:,3));
% Normal acceleration input and bank angle profile follow:
u=-0.5*Omega^2*r*sin(2*y(:,1)).*sin(y(:,3))-2*Omega*v.*sin(y(:,1));
sigma=atan(u/9.81);
