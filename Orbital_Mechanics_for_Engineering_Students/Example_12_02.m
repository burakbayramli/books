% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function Example_12_02
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%  
% This function  solves Example 12.2 by using Encke's method together
% with MATLAB's ode45 to integrate Equation 12.2 for a J2 gravitational
% perturbation given by Equation 12.30.
%
% User M-functions required: sv_from_coe, coe_from_sv, rv_from_r0v0
% User subfunction required: rates 
% ------------------------------------------------------------------------
%...Preliminaries:
clc, close all, clear all

%...Conversion factors:
hours = 3600;                   %Hours to seconds
days  = 24*hours;               %Days to seconds
deg   = pi/180;                 %Degrees to radians

%...Constants:
global   mu
mu     = 398600;                %Gravitational parameter (km^3/s^2)
RE     = 6378;                  %Earth's radius (km)
J2     = 1082.63e-6;    

%...Initial orbital parameters (given):
zp0 = 300;                      %Perigee altitude (km)
za0 = 3062;                     %Apogee altitude (km)
RA0 = 45*deg;                   %Right ascension of the node (radians)
i0  = 28*deg;                   %Inclination (radians)
w0  = 30*deg;                   %Argument of perigee (radians)
TA0 = 40*deg;                   %True anomaly (radians)

%...Initial orbital parameters (inferred):
rp0 = RE + zp0;                 %Perigee radius (km)
ra0 = RE + za0;                 %Apogee radius (km)
e0  = (ra0 - rp0)/(ra0 + rp0);  %Eccentricity
a0  = (ra0 + rp0)/2;            %Semimajor axis (km)
h0  = sqrt(rp0*mu*(1+e0));      %Angular momentum (km^2/s)
T0  = 2*pi/sqrt(mu)*a0^1.5;     %Period (s)

t0  = 0; tf = 2*days;           %Initial and final time (s)
%...end Input data

%Store the initial orbital elements in the array coe0: 
coe0 = [h0 e0 RA0 i0 w0 TA0];

%...Obtain the initial state vector from Algorithm 4.5 (sv_from_coe):
[R0 V0] = sv_from_coe(coe0, mu); %R0 is the initial position vector
                                 %R0 is the initial position vector
r0 = norm(R0); v0 = norm(V0);    %Magnitudes of T0 and V0


del_t = T0/100;                 %Time step for Encke procedure
options = odeset('maxstep', del_t);

%...Begin the Encke integration;
t      = t0;                    %Initialize the time scalar
tsave  = t0;                    %Initialize the vector of solution times
y      = [R0 V0];               %Initialize the state vector
del_y0 = zeros(6,1);            %Initialize the state vector perturbation

t = t + del_t;                  %First time step

%   Loop over the time interval [t0, tf] with equal increments del_t:
while t <= tf + del_t/2
    
%   Integrate Equation 12.7 over the time increment del_t:
    [dum,z] = ode45(@rates, [t0 t], del_y0, options);
    
%   Compute the osculating state vector at time t: 
    [Rosc,Vosc] = rv_from_r0v0(R0, V0, t-t0);

%   Rectify:    
    R0     = Rosc + z(end,1:3);
    V0     = Vosc + z(end,4:6);
    t0     = t;

%   Prepare for next time step:
    tsave  = [tsave;t];
    y      = [y; [R0 V0]];
    t      = t + del_t;
    del_y0 = zeros(6,1); 
end
%   End the loop

t = tsave;   %t is the vector of equispaced solution times
%...End the Encke integration;

%...At each solution time extract the orbital elements from the state
%   vector using Algorithm 4.2:
n_times = length(t);   %n_times is the number of solution times
for j = 1:n_times    
    R     = [y(j,1:3)];
    V     = [y(j,4:6)];
    r(j)  = norm(R);
    v(j)  = norm(V);
    coe   = coe_from_sv(R,V, mu);
    h(j)  = coe(1);
    e(j)  = coe(2);
    RA(j) = coe(3);
    i(j)  = coe(4);
    w(j)  = coe(5);
    TA(j) = coe(6);
end

%...Plot selected osculating elements:

figure(1)
subplot(2,1,1)
plot(t/3600,(RA - RA0)/deg)
title('Variation of Right Ascension')
xlabel('hours')
ylabel('{\it\Delta\Omega} (deg)')
grid on
grid minor
axis tight

subplot(2,1,2)
plot(t/3600,(w - w0)/deg)
title('Variation of Argument of Perigee')
xlabel('hours')
ylabel('{\it\Delta\omega} (deg)')
grid on
grid minor
axis tight

figure(2)
subplot(3,1,1)
plot(t/3600,h - h0)
title('Variation of Angular Momentum')
xlabel('hours')
ylabel('{\it\Deltah} (km^2/s)')
grid on
grid minor
axis tight

subplot(3,1,2)
plot(t/3600,e - e0)
title('Variation of Eccentricity')
xlabel('hours')
ylabel('\it\Deltae')
grid on
grid minor
axis tight

subplot(3,1,3)
plot(t/3600,(i - i0)/deg)
title('Variation of Inclination')
xlabel('hours')
ylabel('{\it\Deltai} (deg)')
grid on
grid minor
axis tight

%...Subfunction:
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function dfdt = rates(t,f)
%~~~~~~~~~~~~~~~~~~~~~~~~~
%
% This function calculates the time rates of Encke's deviation in position
% del_r and velocity del_v.
% -----------------------------------------------------------------------

del_r = f(1:3)';     %Position deviation
del_v = f(4:6)';     %Velocity deviation

%...Compute the state vector on the osculating orbit at time t
%   (Equation 12.5) using Algorithm 3.4:
[Rosc,Vosc] = rv_from_r0v0(R0, V0, t-t0);

%...Calculate the components of the state vector on the perturbed orbit
%   and their magnitudes:
Rpp   = Rosc + del_r;
Vpp   = Vosc + del_v;
rosc  = norm(Rosc);
rpp   = norm(Rpp);

%...Compute the J2 perturbing acceleration from Equation 12.30:
xx    = Rpp(1); yy = Rpp(2); zz = Rpp(3);

fac   =   3/2*J2*(mu/rpp^2)*(RE/rpp)^2;
ap    =   -fac*[(1 - 5*(zz/rpp)^2)*(xx/rpp) ...
                (1 - 5*(zz/rpp)^2)*(yy/rpp) ...
                (3 - 5*(zz/rpp)^2)*(zz/rpp)];

%...Compute the total perturbing ecceleration from Equation 12.7:            
F     = 1 - (rosc/rpp)^3;
del_a = -mu/rosc^3*(del_r - F*Rpp) + ap;

dfdt  = [del_v(1) del_v(2) del_v(3) del_a(1) del_a(2) del_a(3)]';
dfdt  = [del_v del_a]'; %Return the deviative velocity and acceleration
                        %to ode45.

end %rates
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

end %Example_12_02

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    
    


