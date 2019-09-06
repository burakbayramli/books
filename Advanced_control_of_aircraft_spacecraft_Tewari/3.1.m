%Calling program for solving the 2PBVP for optimal aircraft
%navigation by collocation method using MATLABs
%intrinsic code bvp4c.m.
%Requires airnavode.m and airnavbc.m.
%(c) 2010 Ashish Tewari
%dy/dx=f(y,x); a<=x<=b
%y(x=a), y(x=b): Boundary conditions
%y(1,1)= Latitude (rad.)
%y(2,1)= Longitude (rad.)
%y(3,1)= Velocity azimuth (rad.)
%y(4:6,1)=lambda (Lagrange multipliers vector)
global del0; del0=51.5*pi/180; %initial latitude
global lam0;lam0=0; %initial longitude
global delf; delf=40.4*pi/180; %final latitude
global lamf;lamf=-73.5*pi/180; %final longitude
global A0;A0=287.2*pi/180; %initial velocity azimuth
global vw;vw=20; %wind speed
global Aw;Aw=45*pi/180; %wind velocity azimuth
global R; R=0.001; %control cost coefficient
global vprime; vprime=270; %airspeed (m/s)
global r; r=6378.14e3+11000; %radius (m)
global Omega; Omega=7.2921e-5; %Earths rotational rate (rad/s)
tf=6.12*3600; %terminal time

% Collocation points & initial guess follow:
solinit = bvpinit(linspace(0,tf,50),[del0 lam0 A0 0 0 0]);
% 2PBVP solution by collocation method:
sol = bvp4c(@airnavode,@airnavbc,solinit);
x = linspace(0,tf);
y = deval(sol,x);
% Ground speed (m/s) follows:
v=sqrt(vprime^2-vw^2*sin(Aw-y(3,:)).^2)+vw*cos(Aw-y(3,:));
% Normal acceleration input and bank angle profile follow:
u=-y(6,:)./(2*R*v);
sigma=atan(u/9.81);
