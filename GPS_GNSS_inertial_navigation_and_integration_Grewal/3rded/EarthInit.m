%
% Initializes earth model parameters and calculates from them the values of
%             Schuler frequency and period
%             Vertical channel instability time-constant.
%
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
DampingTestF10Psipart1
REarth     = 0.6371009e7;  % Mean Earth radius [m]
OmegaEarth = 0.7292115e-4; % Rotation rate [rad/s]
GMEarth    = 0.3986004e15; % Gravity constant m^3/s^2]
%
tauU         = sqrt(REarth^3/(2*GMEarth)); 
omegaSchuler = sqrt(GMEarth/REarth^3);
fSchuler     = omegaSchuler/2/pi;
TSchuler     = 1/fSchuler;
%
format long
disp(['Mean Earth Radius      = ',num2str(REarth),' [m]']);
disp(['Earth Rotation Rate    = ',num2str(OmegaEarth),' [rad/s]']);
disp(['                       = ',num2str(OmegaEarth*180/pi*3600),' [deg/hr]']);
disp(['Earth Gravity Constant = ',num2str(GMEarth),' [m^3/s^2]']);
disp('Vertical Natigation Instability');
disp(['        Time Constant  = ',num2str(tauU),' [s]']);
disp(['                       = ',num2str(tauU/60),' [min]']);
disp(['Schuler Frequency      = ',num2str(omegaSchuler),' [rad/s]']);
disp(['                       = ',num2str(fSchuler),' [Hz]']);
disp(['Schuler Period         = ',num2str(TSchuler),' [s]']);
disp(['                       = ',num2str(TSchuler/60),' [min]']);
disp(['                       = ',num2str(TSchuler/3660),' [hr]']);
format short;