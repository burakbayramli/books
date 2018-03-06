% -------------------------------------------------------------------------
function r_moon = lunar_position(jd)
%
%...Calculates the geocentric equatorial position vector of the moon
%   given the Julian day.
%
% User M-functions required: None
% -------------------------------------------------------------------------

%...Earth's radius (km):
RE = 6378;

% -------------------------  implementation   -----------------
%...Time in centuries since J2000:
T = (jd - 2451545)/36525;

%...Ecliptic longitude (deg):
e_long =  218.32 + 481267.881*T ...
        + 6.29*sind(135.0 + 477198.87*T) - 1.27*sind(259.3 - 413335.36*T)...
        + 0.66*sind(235.7 + 890534.22*T) + 0.21*sind(269.9 + 954397.74*T)...
        - 0.19*sind(357.5 +  35999.05*T) - 0.11*sind(186.5 + 966404.03*T);
e_long = mod(e_long,360);

%...Ecliptic latitude (deg):   
e_lat  =  5.13*sind( 93.3 + 483202.02*T) + 0.28*sind(228.2 + 960400.89*T)...
        - 0.28*sind(318.3 +   6003.15*T) - 0.17*sind(217.6 - 407332.21*T);
e_lat  = mod(e_lat,360);
    
%...Horizontal parallax (deg):    
h_par  =  0.9508 ...
        + 0.0518*cosd(135.0 + 477198.87*T) + 0.0095*cosd(259.3 - 413335.36*T)...
        + 0.0078*cosd(235.7 + 890534.22*T) + 0.0028*cosd(269.9 + 954397.74*T);
h_par  = mod(h_par,360);

%...Angle between earth's orbit and its equator (deg):
obliquity = 23.439291 - 0.0130042*T;

%...Direction cosines of the moon's geocentric equatorial position vector:
l = cosd(e_lat) * cosd(e_long);
m = cosd(obliquity)*cosd(e_lat)*sind(e_long) - sind(obliquity)*sind(e_lat);
n = sind(obliquity)*cosd(e_lat)*sind(e_long) + cosd(obliquity)*sind(e_lat);

%...Earth-moon distance (km):
dist   = RE/sind(h_par);

%...Moon's geocentric equatorial position vector (km):
r_moon = dist*[l m n];

end  %lunar_position
% -------------------------------------------------------------------------


