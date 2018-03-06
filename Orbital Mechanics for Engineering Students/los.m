%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function light_switch = los(r_sat, r_sun)
%
% This function uses the ECI position vectors of the satellite (r_sat)
% and the sun (r_sun) to determine whether the earth is in the line of
% sight between the two.
%
% User M-functions required: None
%--------------------------------------------------------------------------
RE        = 6378;          %Earth's radius (km)
rsat      = norm(r_sat);
rsun      = norm(r_sun);

%...Angle between sun and satellite position vectore:  
theta     = acosd(dot(r_sat, r_sun)/rsat/rsun);

%...Angle between the satellite position vector and the radial to the point
%   of tangency with the earth of a line from the satellite:                                              
theta_sat = acosd(RE/rsat);

%...Angle between the sun position vector and the radial to the point
%   of tangency with the earth of a line from the sun: 
theta_sun = acosd(RE/rsun);

%...Determine whether a line from the sun to the satellite 
%   intersects the earth:
if theta_sat + theta_sun <= theta
    light_switch = 0;   %yes
else
    light_switch = 1;   %no
end

end %los
%--------------------------------------------------------------------------
