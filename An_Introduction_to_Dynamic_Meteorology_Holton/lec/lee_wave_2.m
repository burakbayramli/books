% MATLAB file: lee_wave_2.m
% Lee-wave in 1-d stratified model for broad
% ridge defined by the function h(x) = hm/(1+(x/L)^2).
% Approximate solution for wide ridge case.  
clear
close all 
disp('topographic wave for isolated ridge')
disp('ridge profile  h(x) = hm/(1+(x/L)^2 ')
%  Set parameters
bv = 1.e-4;                 % buoyancy frequency squared
g = 9.81;
%
U = input('give mean zonal wind in m/s  ')
L = 20;    		            % zonal scale of ridge in km  
Lx = 360 ;                  % Lx is the length of the domain in km 
k = 2*pi/(Lx*1.e3) ;        % lowest zonal wavenumber in m
ls = sqrt(bv/U^2)*1.e3;     % Scorer parameter in 1/km
lzlength = 2*pi/ls          % vertical wavelength
Lz = lzlength;              % depth of domain
%%*********************************************
%This version gives approximate analytic solution for vertical parcel
%displacement in wide ridge case
%%*********************************************
NL = 61;	            	% number of vertical gridpoints
N = 128;                    % number of horizontal gridpoints
zz = linspace(0,Lz,NL);
xx = linspace(-Lx/2, Lx/2, N); 
dz = Lz/(NL-1);
s = [1:N];
xm = 0;                     % location of ridge in km
hxx = (Lz/8)*L^2./(L^2+((xx-xm)).^2);   %topographic height profile in km
[x, z] = meshgrid(xx,zz); 
[hx,z] = meshgrid(hxx,zz);
dpz = hx/L.*(L*cos(ls*z)-(x-xm).*sin(ls*z));   % displacement height
stream = z-dpz;             %  streamlines
figure(1)
  [cs H] = contour(x,z/Lz,dpz/Lz);
    clabel(cs,H)
  title('vertical displacement height in fractions of vertical wavelength')
  xlabel( 'horizontal distance (km)')
  ylabel('height   (vertical wavelengths)')
figure(2)
  [cs H] = contour(x,z/Lz,stream/Lz, [0 1/8 2/8 3/8 4/8 5/8 6/8 7/8]);
    clabel(cs,H)
  title('streamlines for analytic solution (units of mean nondimensioinal height)')
  xlabel( 'horizontal distance (km)')
  ylabel('height   (vertical wavelengths)')