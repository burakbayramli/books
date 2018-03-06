% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function Example_12_6
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
% This function solves Example 12.6 by using MATLAB's ode45 to numerically
% integrate Equations 12.89 (the Gauss planetary equations) to determine
% the J2 perturbation of the orbital elements.
%
% User M-functions required:  None
% User subfunctions required: rates                          
% ------------------------------------------------------------------------

%...Preliminaries:
close all; clear all; clc

%...Conversion factors:
hours   = 3600;                    %Hours to seconds
days    = 24*hours;                %Days to seconds
deg     = pi/180;                  %Degrees to radians

%...Constants:    
mu      = 398600;                  %Gravitational parameter (km^3/s^2)
RE      = 6378;                    %Earth's radius (km)
J2      = 1082.63e-6;              %Earth's J2

%...Initial orbital parameters (given):
rp0     = RE + 300;                %perigee radius (km)
ra0     = RE + 3062;               %apogee radius (km
RA0     = 45*deg;                  %Right ascencion of the node (radians)
i0      = 28*deg;                  %Inclination (radians)
w0      = 30*deg;                  %Argument of perigee (radians)
TA0     = 40*deg;                  %True anomaly (radians)

%...Initial orbital parameters (inferred):
e0      = (ra0 - rp0)/(ra0 + rp0); %eccentricity
h0      = sqrt(rp0*mu*(1 + e0));   %angular momentrum (km^2/s)
a0      = (rp0 + ra0)/2;           %Semimajor axis (km)
T0      = 2*pi/sqrt(mu)*a0^1.5;    %Period (s)

%...Store initial orbital elements (from above) in the vector coe0:
coe0    = [h0 e0 RA0 i0 w0 TA0];

%...Use ODE45 to integrate the Gauss variational equations (Equations
%   12.89) from t0 to tf:
t0      = 0;
tf      = 2*days;
nout    = 5000;  %Number of solution points to output for plotting purposes
tspan   = linspace(t0, tf, nout);
options = odeset(...
                 'reltol',      1.e-8, ...
                 'abstol',      1.e-8, ...
                 'initialstep', T0/1000);  
y0     = coe0'; 
[t,y]  = ode45(@rates, tspan, y0, options);

%...Assign the time histories mnemonic variable names:
h  = y(:,1);
e  = y(:,2);
RA = y(:,3);
i  = y(:,4);
w  = y(:,5);
TA = y(:,6);

%...Plot the time histories of the osculatinig elements:
figure(1)
subplot(5,1,1)
plot(t/3600,(RA - RA0)/deg)
title('Right Ascension (degrees)')
xlabel('hours')
grid on
grid minor
axis tight

subplot(5,1,2)
plot(t/3600,(w - w0)/deg)
title('Argument of Perigee (degrees)')
xlabel('hours')
grid on
grid minor
axis tight

subplot(5,1,3)
plot(t/3600,h - h0)
title('Angular Momentum (km^2/s)')
xlabel('hours')
grid on
grid minor
axis tight

subplot(5,1,4)
plot(t/3600,e - e0)
title('Eccentricity')
xlabel('hours')
grid on
grid minor
axis tight

subplot(5,1,5)
plot(t/3600,(i - i0)/deg)
title('Inclination (degrees)')
xlabel('hours')
grid on
grid minor
axis tight

%...Subfunction:

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function dfdt = rates(t,f)
%~~~~~~~~~~~~~~~~~~~~~~~~~
%
% This function calculates the time rates of the orbital elements
% from Gauss's variational equations (Equations 12.89).
%-------------------------------------------------------------------------

%...The orbital elements at time t:
h     = f(1);
e     = f(2);
RA    = f(3);
i     = f(4);
w     = f(5);
TA    = f(6);

r     = h^2/mu/(1 + e*cos(TA)); %The radius
u     = w + TA;                 %Argument of latitude

%...Orbital element rates at time t (Equations 12.89):
hdot  = -3/2*J2*mu*RE^2/r^3*sin(i)^2*sin(2*u);

edot  = ...
    3/2*J2*mu*RE^2/h/r^3*(h^2/mu/r ...
   *(sin(u)*sin(i)^2*(3*sin(TA)*sin(u) - 2*cos(TA)*cos(u)) - sin(TA)) ...
   -sin(i)^2*sin(2*u)*(e + cos(TA)));

edot  = 3/2*J2*mu*RE^2/h/r^3 ...
        *(h^2/mu/r*sin(TA)*(3*sin(i)^2*sin(u)^2 - 1) ...
          -sin(2*u)*sin(i)^2*((2+e*cos(TA))*cos(TA)+e)); 
          
TAdot =  h/r^2 + 3/2*J2*mu*RE^2/e/h/r^3 ...
         *(h^2/mu/r*cos(TA)*(3*sin(i)^2*sin(u)^2 - 1) ...
         + sin(2*u)*sin(i)^2*sin(TA)*(h^2/mu/r + 1));
     
RAdot = -3*J2*mu*RE^2/h/r^3*sin(u)^2*cos(i);

idot  = -3/4*J2*mu*RE^2/h/r^3*sin(2*u)*sin(2*i);
       
wdot  = 3/2*J2*mu*RE^2/e/h/r^3 ...
        *(-h^2/mu/r*cos(TA)*(3*sin(i)^2*sin(u)^2 - 1) ...
          - sin(2*u)*sin(i)^2*sin(TA)*(2 + e*cos(TA)) ...
          + 2*e*cos(i)^2*sin(u)^2); 
      
%...Pass these rates back to ODE45 in the array dfdt:
dfdt  =  [hdot edot RAdot idot wdot TAdot]';

end %rates
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

end %Example_12_6
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~