% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function Example_12_09
%
% This function solve Example 12.9 the Gauss planetary equations for
% solar radiation pressure (Equations 12.106).
%
% User M-functions required:  sv_from_coe, los, solar_position
% User subfunctions required: rates
% The M-function rsmooth may be found in Garcia (2010).
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

global JD  %Julian day

%...Preliminaries:
close all
clear all
clc

%...Conversion factors:
hours = 3600;                  %Hours to seconds
days  = 24*hours;              %Days to seconds
deg   = pi/180;                %Degrees to radians
 
%...Constants;
mu    = 398600;                %Gravitational parameter (km^3/s^2)
RE    = 6378;                  %Eath's radius (km)
c     = 2.998e8;               %Speed of light (m/s)
S     = 1367;                  %Solar constant (W/m^2)
Psr   = S/c;                   %Solar pressure (Pa);

%...Satellite data:                  
CR    = 2;                     %Radiation pressure codfficient
m     = 100;                   %Mass (kg)
As    = 200;                   %Frontal area (m^2);

%...Initial orbital parameters (given):
a0    = 10085.44;              %Semimajor axis (km)
e0    = 0.025422;              %eccentricity
incl0 = 88.3924*deg;           %Inclination (radians)
RA0   = 45.38124*deg;          %Right ascencion of the node (radians)
TA0   = 343.4268*deg;          %True anomaly (radians)
w0    = 227.493*deg;           %Argument of perigee (radians)

%...Initial orbital parameters (inferred):
h0    = sqrt(mu*a0*(1-e0^2));  %angular momentrum (km^2/s)
T0    = 2*pi/sqrt(mu)*a0^1.5;  %Period (s)
rp0   = h0^2/mu/(1 + e0);      %perigee radius (km)
ra0   = h0^2/mu/(1 - e0);      %apogee radius (km)

%...Store initial orbital elements (from above) in the vector coe0:
coe0  = [h0 e0 RA0 incl0 w0 TA0];

%...Use ODE45 to integrate Equations 12.106, the Gauss planetary equations  
%   from t0 to tf:

JD0   = 2438400.5;              %Initial Julian date (6 January 1964 0 UT)
t0    = 0;                      %Initial time (s)        
tf    = 3*365*days;             %final time (s)
y0    = coe0';                  %Initial orbital elements
nout  = 4000;                   %Number of solution points to output
tspan = linspace(t0, tf, nout); %Integration time interval
options = odeset(...
                 'reltol',      1.e-8, ...
                 'abstol',      1.e-8, ...
                 'initialstep', T0/1000);
[t,y] = ode45(@rates, tspan, y0, options);

%...Extract or compute the orbital elements' time histories from the
%   solution vector y:
h     = y(:,1);
e     = y(:,2);
RA    = y(:,3);
incl  = y(:,4);
w     = y(:,5);
TA    = y(:,6);
a     = h.^2/mu./(1 - e.^2);

%...Smooth the data to remove short period variations: 
h     = rsmooth(h);
e     = rsmooth(e);
RA    = rsmooth(RA);
incl  = rsmooth(incl);
w     = rsmooth(w);
a     = rsmooth(a);

figure(2)
subplot(3,2,1)
plot(t/days,h - h0)
title('Angular Momentum (km^2/s)')
xlabel('days')
axis tight

subplot(3,2,2)
plot(t/days,e - e0)
title('Eccentricity')
xlabel('days')
axis tight

subplot(3,2,4)
plot(t/days,(RA - RA0)/deg)
title('Right Ascension (deg)')
xlabel('days')
axis tight

subplot(3,2,5)
plot(t/days,(incl - incl0)/deg)
title('Inclination (deg)')
xlabel('days')
axis tight

subplot(3,2,6)
plot(t/days,(w - w0)/deg)
title('Argument of Perigee (deg)')
xlabel('days')
axis tight

subplot(3,2,3)
plot(t/days,a - a0)
title('Semimajor axis (km)')
xlabel('days')
axis tight

%...Subfunctions:
%~~~~~~~~~~~~~~~~~~~~~~~~~
function dfdt = rates(t,f)
%~~~~~~~~~~~~~~~~~~~~~~~~~
%...Update the Julian Date at time t:
JD     =   JD0 + t/days;

%...Compoute the apparent position vector of the sun:
[lamda eps r_sun] = solar_position(JD);

%...Convert the ecliptic latitude and the obliquity to radians:
lamda  = lamda*deg;  
eps    = eps*deg;

%...Extract the orbital elements at time t
h      = f(1);
e      = f(2);
RA     = f(3);
i      = f(4);
w      = f(5);
TA     = f(6);

u      = w + TA;  %Argument of latitude

%...Compute the state vector at time t:
coe    =   [h e RA i w TA];
[R, V] =   sv_from_coe(coe,mu);

%...Calculate the manitude of the radius vector:
r      =   norm(R);

%...Compute the shadow function and the solar radiation perturbation:
nu     =   los(R, r_sun);
pSR    =   nu*(S/c)*CR*As/m/1000;

%...Calculae the trig functions in Equations 12.105.
sl = sin(lamda);  cl = cos(lamda);
se = sin(eps);    ce = cos(eps);
sW = sin(RA);     cW = cos(RA);
si = sin(i);      ci = cos(i);
su = sin(u);      cu = cos(u);
sT = sin(TA);     cT = cos(TA);


%...Calculate the earth-sun unit vector components (Equations 12.105):
ur     =   sl*ce*cW*ci*su + sl*ce*sW*cu - cl*sW*ci*su ...
         + cl*cW*cu + sl*se*si*su;
     
us     =   sl*ce*cW*ci*cu - sl*ce*sW*su - cl*sW*ci*cu ...
         - cl*cW*su + sl*se*si*cu;
     
uw     = - sl*ce*cW*si + cl*sW*si + sl*se*ci;  

%...Calculate the time rates of the osculating elements from
%   Equations 12.106:
    
hdot   = -pSR*r*us;

edot   = -pSR*(h/mu*sT*ur ...
               + 1/mu/h*((h^2 + mu*r)*cT + mu*e*r)*us);
     
TAdot  =   h/r^2 ...
         - pSR/e/h*(h^2/mu*cT*ur - (r + h^2/mu)*sT*us);
     
RAdot  = -pSR*r/h/si*su*uw;

idot   = -pSR*r/h*cu*uw;

wdot   = -pSR*(-1/e/h*(h^2/mu*cT*ur - (r + h^2/mu)*sT*us) ...
              - r*su/h/si*ci*uw);  
          
%...Return the rates to ode45:
dfdt   =  [hdot edot RAdot idot wdot TAdot]';

end %rates

end %Example_12_9

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
