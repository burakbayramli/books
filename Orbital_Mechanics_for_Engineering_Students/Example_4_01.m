% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Example 4.1
% ~~~~~~~~~~~
%{
  This program calculates the right ascension and declination
  from the geocentric equatorial position vector using the data
  in Example 4.1.

  r   - position vector r (km)
  ra  - right ascension (deg)
  dec - declination (deg)

  User M-functions required: ra_and_dec_from_r

%}
% -----------------------------------------------

clear all; clc

r        = [-5368 -1784 3691];
[ra dec] = ra_and_dec_from_r(r);

fprintf('\n -----------------------------------------------------\n')
fprintf('\n Example 4.1\n')
fprintf('\n r               = [%g  %g  %g] (km)', r(1), r(2), r(3))
fprintf('\n right ascension = %g deg', ra)
fprintf('\n declination     = %g deg', dec) 
fprintf('\n\n -----------------------------------------------------\n')

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~