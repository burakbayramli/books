% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  function Example_12_01
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% This function solves Example 12.1 by using MATLAB's ode45 to numerically
% integrate Equation 12.2 for atmospheric drag.
%
% User M-functions required:  sv_from_coe, atmosphere
% User subfunctions required: rates, terminate                           
% ------------------------------------------------------------------------

%...Preliminaries:
close all, clear all, clc

%...Conversion factors:
hours    = 3600;                   %Hours to seconds
days     = 24*hours;               %Days to seconds
deg      = pi/180;                 %Degrees to radians
 
%...Constants;
mu       = 398600;                 %Gravitational parameter (km^3/s^2)
RE       = 6378;                   %Earth's radius (km)
wE       = [ 0 0 7.2921159e-5]';   %Earth's angular velocity (rad/s)

%...Satellite data:                  
CD       = 2.2;                    %Drag codfficient
m        = 100;                    %Mass (kg)
A        = pi/4*(1^2) ;            %Frontal area (m^2)

%...Initial orbital parameters (given):
rp       = RE + 215;               %perigee radius (km)
ra       = RE + 939;               %apogee radius (km)
RA       = 339.94*deg;             %Right ascencion of the node (radians)
i        = 65.1*deg;               %Inclination (radians)
w        = 58*deg;                 %Argument of perigee (radians)
TA       = 332*deg;                %True anomaly (radians)

%...Initial orbital parameters (inferred):
e        = (ra-rp)/(ra+rp);        %eccentricity
a        = (rp + ra)/2;            %Semimajor axis (km)
h        = sqrt(mu*a*(1-e^2));     %angular momentrum (km^2/s)
T        = 2*pi/sqrt(mu)*a^1.5;    %Period (s)

%...Store initial orbital elements (from above) in the vector coe0:
coe0     = [h e RA i w TA];

%...Obtain the initial state vector from Algorithm 4.5 (sv_from_coe):
[R0 V0]  = sv_from_coe(coe0, mu);  %R0 is the initial position vector
                                   %V0 is the initial velocity vector
r0 = norm(R0); v0 = norm(V0);      %Magnitudes of R0 and V0

%...Use ODE45 to integrate the equations of motion d/dt(R,V) = f(R,V) 
%   from t0 to tf:
t0       = 0; tf = 120*days;       %Initial and final times (s)
y0       = [R0 V0]';               %Initial state vector
nout     = 40000;                  %Number of solution points to output
tspan    = linspace(t0, tf, nout); %Integration time interval

%   Set error tolerances, initial step size, and termination event:
options  = odeset('reltol',      1.e-8, ...
                  'abstol',      1.e-8, ...
                  'initialstep', T/10000,    ...
                  'events',      @terminate);
global alt %Altitude             
[t,y]    = ode45(@rates, tspan, y0,options); %t is the solution times
                                             %y is the state vector history

%...Extract the locally extreme altitudes:
altitude = sqrt(sum(y(:,1:3).^2,2)) - RE;    %Altitude at each time
[max_altitude,imax,min_altitude,imin] = extrema(altitude);
maxima   = [t(imax) max_altitude];  %Maximum altitudes and times
minima   = [t(imin) min_altitude];  %Minimum altitudes and times
apogee   = sortrows(maxima,1);      %Maxima sorted with time                       
perigee  = sortrows(minima,1);      %Minima sorted with time

figure(1)
apogee(1,2) = NaN;
%...Plot perigee and apogee history on the same figure:
plot(apogee(:,1)/days, apogee(:,2),'b','linewidth',2)
hold on
plot(perigee(:,1)/days, perigee(:,2),'r','linewidth',2)
grid on
grid minor
xlabel('Time (days)')
ylabel('Altitude (km)')
ylim([0 1000]);

%...Subfunctions:
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  function dfdt = rates(t,f) 
% ~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% This function calculates the spacecraft acceleration from its
% position and velocity at time t.
% ------------------------------------------------------------------------
R    = f(1:3)';            %Position vector (km/s)
r    = norm(R);            %Distance from earth's center (km)
alt  = r - RE;             %Altitude (km)
rho  = atmosphere(alt);    %Air density from US Standard Model (kf/m^3)
V    = f(4:6)';            %Velocity vector (km/s)
Vrel = V - cross(wE,R);    %Velocity relative to the atmosphere (km/s)
vrel = norm(Vrel);         %Speed relative to the atmosphere (km/s)
uv   = Vrel/vrel;          %Relative velocity unit vector
ap   = -CD*A/m*rho*...     %Acceleration due to drag (m/s^2)
       (1000*vrel)^2/2*uv; %(converting units of vrel from km/s to m/s)
a0   = -mu*R/r^3;          %Gravitational ecceleration (km/s^2)      
a    = a0 + ap/1000;       %Total acceleration (km/s^2)
dfdt = [V a]';             %Velocity and the acceleraion returned to ode45                 
end %rates
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  function [lookfor stop direction] = terminate(t,y)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% This function specifies the event at which ode45 terminates.
% ------------------------------------------------------------------------
lookfor   = alt - 100; % = 0 when altitude = 100 km
stop      = 1;         %  1 means terminate at lookfor = 0; Otherwise 0
direction = -1;        % -1 means zero crossing is from above
end %terminate
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

end %Example_12_01
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~