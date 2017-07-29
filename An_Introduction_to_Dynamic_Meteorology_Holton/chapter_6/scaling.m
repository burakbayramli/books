%
% scaling parameters to convert non-dimensional variables into dimensional variables
%
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-09 09:31:05 -0800 (Thu, 09 Feb 2012) $
% $Revision: 788 $
% $Author: hakim $
% $Id: scaling.m 788 2012-02-09 17:31:05Z hakim $

% this scaling gives a Burger # of 1
rhonot = 1.; % kg/m^3; density
f = 1e-4; % s^-1; Coriolis parameter
U = 10;   % m/s; horizontal velocity scale
L = 1e6;  % m; horizontal length scale
N = 1.e-2; % s^-1; bouyancy frequency
g = 9.81; % m/s
Thnot = 300; % K; constant reference surface potential temperature
Pnot = 1000*100; % Pa; constant reference surface pressure

% derived variables
H = f*L/N;  % m; vertical length scale
Tstar = L/U;  % s; time scaling
Ro = U/(f*L); % unitless; Rossby number
Pstar = rhonot*U*f*L; % Pa; pressure scaling
Qstar = U/L; % QG PV and vorticity scaling
Thstar = U*f*L*Thnot/(g*H); % K; potential temperature scaling
Wstar = Ro*H*U/L; % m/s; vertical velocity scaling
Thbz = Thnot*N*N/g; % K/m; scaling for d\theta/dz
EPVstar = Qstar*Thbz/rhonot; % m^2 K kg^-1 s^-1; Ertel PV scaling for QG PV

% misc constants
R = 287;
Cp = 1004;

% conversion
km = 1e3; % m -> km
hPa = 1e2; % Pa -> hPa
minute = 60; % s -> min
hr = 3600; % s -> hour
day = 86400; % s -> day
pvu = 1e6; % "PV units"
