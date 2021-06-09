% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function Example_12_11
% This function solves Example 12.11 by using MATLAB's ode45 to integrate
% Equations 12.84, the Gauss variational equations, for a lunar
% gravitational perturbation.
%
% User M-functions required:  sv_from_coe, lunar_position
% User subfunctions required: solveit, rates
% ------------------------------------------------------------------------

global JD   %Julian day

%...Preliminaries:
close all
clear all
clc

%...Conversion factors:
hours    = 3600;               %Hours to seconds
days     = 24*hours;           %Days to seconds
deg      = pi/180;             %Degrees to radians
 
%...Constants;
mu       = 398600;             %Earth's gravitational parameter (km^3/s^2)
mu3      = 4903;               %Moon's gravitational parameter (km^3/s^2)
RE       = 6378;               %Earth's radius (km)

%...Initial data for each of the three given orbits:
  
type = {'GEO' 'HEO' 'LEO'};

%...GEO
n    = 1;
a0   = 42164;    %semimajor axis (km)
e0   = 0.0001;   %eccentricity
w0   = 0;        %argument of perigee (rad)
RA0  = 0;        %right ascension (rad)
i0   = 1*deg;    %inclination (rad)
TA0  = 0;        %true anomaly (rad)
JD0  = 2454283;   %Julian Day
solveit

%...HEO
n    = 2;
a0   = 26553.4;
e0   = 0.741;
w0   = 270;
RA0  = 0;
i0   = 63.4*deg;
TA0  = 0;
JD0  = 2454283;
solveit

%...LEO
n    = 3;
a0   = 6678.136;
e0   = 0.01;
w0   = 0;
RA0  = 0;
i0   = 28.5*deg;
TA0  = 0;
JD0  = 2454283;
solveit

%...Subfunctions:

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function solveit
%
% Calculations and plots common to all of the orbits
% 
%---------------------------------------------------
%
%...Initial orbital parameters (calculated from the given data):
h0   = sqrt(mu*a0*(1-e0^2));  %angular momentum (km^2/s)
T0   = 2*pi/sqrt(mu)*a0^1.5;  %Period (s)
rp0  = h0^2/mu/(1 + e0);      %perigee radius (km)
ra0  = h0^2/mu/(1 - e0);      %apogee radius (km)

%...Store initial orbital elements (from above) in the vector coe0:
coe0 = [h0;e0;RA0;i0;w0;TA0];

%...Use ODE45 to integrate the Equations 12.84, the Gauss variational
%   equations with lunar gravity as the perturbation, from t0 to tf:
t0       = 0; 
tf       = 60*days;
y0       = coe0;                   %Initial orbital elements
nout     = 400;                    %Number of solution points to output
tspan    = linspace(t0, tf, nout); %Integration time interval
options = odeset(...
                 'reltol', 1.e-8, ...
                 'abstol', 1.e-8);
[t,y]   = ode45(@rates, tspan, y0, options);

%...Time histories of the right ascension, inclination and argument of
%   perigee:
RA = y(:,3);   
i  = y(:,4);
w  = y(:,5);

%...Smooth the data to eliminate short period variations:
RA = rsmooth(RA);
i  = rsmooth(i);
w  = rsmooth(w);

figure(n)
subplot(1,3,1)
plot(t/days,(RA - RA0)/deg)
title('Right Ascension vs Time')
xlabel('{\itt} (days)')
ylabel('{\it\Omega} (deg)')
axis tight

subplot(1,3,2)
plot(t/days,(i - i0)/deg)
title('Inclination vs Time')
xlabel('{\itt} (days)')
ylabel('{\iti} (deg)')
axis tight

subplot(1,3,3)
plot(t/days,(w - w0)/deg)
title('Argument of Perigee vs Time')
xlabel('{\itt} (days)')
ylabel('{\it\omega} (deg)')
axis tight

drawnow

end %solveit
%~~~~~~~~~~~~~~~~~~~~~~~~~

%~~~~~~~~~~~~~~~~~~~~~~~~~
function dfdt = rates(t,f)
%~~~~~~~~~~~~~~~~~~~~~~~~~
%...The orbital elements at time t:
h      =   f(1);
e      =   f(2);
RA     =   f(3);
i      =   f(4);
w      =   f(5);
TA     =   f(6);
phi    =   w + TA; %argument of latitude

%...Obtain the state vector at time t from Algorithm 4.5:
coe    =   [h e RA i w TA];
[R, V] =   sv_from_coe(coe,mu);

%...Obtain the unit vectors of the rsw system:
r      =   norm(R);
ur     =   R/r;            %radial
H      =   cross(R,V);
uh     =   H/norm(H);      %normal
s      =   cross(uh, ur);
us     =   s/norm(s);      %transverse

%...Update the Julian Day:
JD     =   JD0 + t/days;

%...Find and normalize the position vector of the moon:
R_m    =   lunar_position(JD);
r_m    =   norm(R_m);

R_rel  =   R_m - R;     %R_rel = position vector of moon wrt satellite
r_rel  =   norm(R_rel);

%...See Appendix F:
q      =   dot(R,(2*R_m - R))/r_m^2;
F      =   (q^2 - 3*q + 3)*q/(1 + (1-q)^1.5);

%...Gravitationl perturbation of the moon (Equation 12.117):
ap     =   mu3/r_rel^3*(F*R_m - R);

%...Perturbation components in the rsw system:
apr    =   dot(ap,ur);
aps    =   dot(ap,us);
aph    =   dot(ap,uh);

%...Gauss variational equations (Equations 12.84):
hdot   =   r*aps;

edot   =   h/mu*sin(TA)*apr ...
         + 1/mu/h*((h^2 + mu*r)*cos(TA) + mu*e*r)*aps;
     
RAdot  =   r/h/sin(i)*sin(phi)*aph;

idot   =   r/h*cos(phi)*aph;

wdot   = - h*cos(TA)/mu/e*apr ...
         + (h^2 + mu*r)/mu/e/h*sin(TA)*aps ...
         - r*sin(phi)/h/tan(i)*aph;

TAdot  = h/r^2 ...
         + 1/e/h*(h^2/mu*cos(TA)*apr - (r + h^2/mu)*sin(TA)*aps);

%...Return rates to ode45 in the array dfdt:     
dfdt   =  [hdot edot RAdot idot wdot TAdot]';

end %rates
%~~~~~~~~~~~~~~~~~~~~~~~~~

end %Example_12_11
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~