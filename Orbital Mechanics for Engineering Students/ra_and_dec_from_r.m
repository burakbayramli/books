% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function [ra dec] = ra_and_dec_from_r(r)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%{  
  This function calculates the right ascension and the
  declination from the geocentric equatorial position vector

  r       - position vector
  l, m, n - direction cosines of r
  ra      - right ascension (degrees)
  dec     - declination (degrees)
%}
% ----------------------------------------------
l = r(1)/norm(r);
m = r(2)/norm(r);
n = r(3)/norm(r);

dec = asind(n);

if m > 0
    ra = acosd(l/cosd(dec));
else
    ra = 360 - acosd(l/cosd(dec));
end
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~