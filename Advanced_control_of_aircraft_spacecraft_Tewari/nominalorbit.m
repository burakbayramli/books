% Program 'nominalorbit.m' for calculating radius,
% radial speed, and angular momentum of spacecraft in a
% nominal elliptical orbit.
% Requires 'kepler.m'
% (c) 2009 Ashish Tewari
function [r,rdot,h]=nominalorbit(t);
mu=398600.4; %Gravitational constant of planet (km^3/s^2)
a=7000; % Semi-major axis of nominal orbit (km)
e=0.5; % Eccentricity of nominal orbit (Elliptical)
t0=-2000; % Time of periapsis of nominal orbit (s)
p=a*(1-e^2); % Parameter of nominal orbit (km)
h=sqrt(mu*p); % Angular momentum of nominal orbit (km^2/s)
n=sqrt(mu/a^3); % Mean motion of nominal orbit (rad/s)
M=n*(t-t0); % Mean anomaly in the nominal orbit (rad.)
E=kepler(e,M); % Eccentric anomaly in the nominal orbit (rad.)
r=a*(1-e*cos(E)); % Nominal radius (km)
rdot=e*sqrt(mu*a)*sin(E)/r; % Nominal radial speed (km/s)
