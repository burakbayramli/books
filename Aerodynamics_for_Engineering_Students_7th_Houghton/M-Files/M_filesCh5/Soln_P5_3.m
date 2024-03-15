%
% Chapter 5 homework solutions via MATLAB
%
% Problem 5.3: 
clear;clc
mso = 40; % m^2/s; 
msi = 20;
U = 2.29183118; % m/s (this was adjusted until ux = 0) 
%
% GRAPHICAL SOLUTION:
%
x = 0; 
y = 2.0; % 3-4-5 triangle ==> dx = 1.5. dy = 2, dh = 2.5
         % NOTE: dh = sqrt(dx^2 + dy^2) ==> distance given 
         % from source and sink to point equidistant by 2.5 
% Stream function differentiuated to get ux and uu:
% psis = U*y - (msi/2/pi)*atan2(y,x+1.5) + (mso/2/pi)*atan2(y,x-1.5);
% Horizontal component of veliocity vector:
       ux = U - (msi/2/pi) * 1/((x+1.5)+(y^2/(x+1.5)) ) ...
           + (mso/2/pi) * 1/( (x-1.5)+(y^2/(x-1.5)) );
% Vertical component of velocity vector:
       uy = (msi/2/pi) * 1/(1+(y^2/(x+1.5)^2))*(-y/(x-1.5)^2) ...
           - (mso/2/pi) * 1/(1+(y^2/(x-1.5)^2))*(-y/(x+1.5)^2);
 disp(' ')
 disp('Solution to Problem 3 (Chapter 3):')
 disp(' Computation of (ux,uy) at (x,y) = (0,2) ') 
 disp('    U (m/s)   ux (m/s)   uy (m/s)' )
 disp([ U ux uy ])