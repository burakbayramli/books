%
% Compares parameteric, geocentric and geodetic latitudes on WGS 84 model
%
clear all;
close all;
deg = pi/180;
a = 6378137;                            % semi-major (equatorial) radius [meter]
b = 6356752.3142;                       % semi-minor (polar) radius [meter]
k = 0;                                  % indexing variable
for psi=-90:90,                         % psi is parametric latitude in degrees
   k      = k+1;                        % increment index
   spsi   = sin(psi*deg);               % sine of parametric latitude
   cpsi   = cos(psi*deg);               % cosine of parametric latitude
   p(k)   = psi;                        % parametric latitude [deg]
   c(k)   = atan2(b*spsi,a*cpsi)/deg;   % geocentric latitude [deg]
   d(k)   = atan2(a*spsi,b*cpsi)/deg;   % geodetic latitude [deg]
end;
figure;
plot(p,c-p,'k-',p,d-p,'k:');            % plot results
legend('Geocentric','Geodetic');
xlabel('Parametric Latitude [deg]');
ylabel('Latitude Difference [deg]');
title('WGS 84 Latitude Differences');
