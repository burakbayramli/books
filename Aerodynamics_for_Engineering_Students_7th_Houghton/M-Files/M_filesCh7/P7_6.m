% Solution to Problem 5.6 --------------------------------
% in Chapter 5 of "Aerodynamics for Engineering Students", 
% 6th Edition (2013) by E. L. Houghton, et al. 
% This code was written by Daniel T. Valentine, Fall 2012
   clear;clc;
%
  rho = 1.2256; % Density of fluid, kg/m^3.
  V = 1;        % Free-stream speed, m/s.
% Wing geometric characteristics:
  s = 3/2;      % Wing semi-span, m.
  b = 2*s;      % Wing span.
  AR = 3;       % Aspect ratio.
  S = b^2/AR;   % Plan area.
  cbar = S/b;   % Standard mean chord (SMC).
% Fourier analysis of circulation distribution:
% Two-term expansion as requested in the problem statement:
NZ = 2; 
theta = 0:pi/2/NZ:pi/2;
cc = 1;  
ainff = 6;
alphaGive = 1*180/pi;  % alpha in degrees
sinthe = sin(theta);
muu = cc.*ainff/(8*s); % s, in this code, is semi span.
mualphsin = muu.*alphaGive.*sinthe*pi/180;
for mm = 1:NZ+1
     mt = mm;
    for nz = 1:NZ+1
        nn = 2*nz - 1;
CC(mm,nz) = sin(nn*theta(mt))*(sinthe(mt) + nn*muu);
    end
end
for mm = 1:NZ
    for nn=1:NZ
        C(mm,nn) = CC(mm+1,nn);
    end
end
C;
b = mualphsin(2:end);
Cinv = inv(C);
A = C\b'
%
