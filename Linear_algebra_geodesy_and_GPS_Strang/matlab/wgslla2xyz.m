% Function xyz = wgslla2xyz(lat, lon, alt) returns the 
% equivalent WGS84 XYZ coordinates (in meters) for a
% given geodetic latitude "lat" (degrees), longitude "lon" 
% (degrees), and altitude above the WGS84 ellipsoid
% in meters.  Note: N latitude is positive, S latitude
% is negative, E longitude is positive, W longitude is
% negative.


function xyz = wgslla2xyz(LLA)

    wlat = LLA(1);
    wlon = LLA(2);
    walt = LLA(3);

	A_EARTH = 6378137;
	flattening = 1/298.257223563;
	NAV_E2 = (2-flattening)*flattening; % also e^2
	deg2rad = pi/180;

	slat = sin(wlat*deg2rad);
	clat = cos(wlat*deg2rad);
	r_n = A_EARTH/sqrt(1 - NAV_E2*slat*slat);
	xyz = [ (r_n + walt)*clat*cos(wlon*deg2rad);  
	        (r_n + walt)*clat*sin(wlon*deg2rad);  
	        (r_n*(1 - NAV_E2) + walt)*slat ];

	if ((wlat < -90.0) | (wlat > +90.0) |...
				(wlon < -180.0) | (wlon > +360.0))
		error('WGS lat or WGS lon out of range');
        end
return


