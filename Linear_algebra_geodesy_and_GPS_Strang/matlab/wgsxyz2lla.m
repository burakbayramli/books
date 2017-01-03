% Function [lat, lon, alt] = wgsxyz2lla(xyz) transforms the
% 3 x 1 vector xyz representing a point in WGS84 xyz coordinates
% into its corresponding latitude (degrees), longitude (degrees),
% and height above WGS84 ellipsoid (in meters)
%


function lla = wgsxyz2lla(xyz)

        %  This dual-variable iteration seems to be 7 or 8 times faster than
	%  a one-variable (in latitude only) iteration.  AKB 7/17/95 

	A_EARTH = 6378137;
	flattening = 1/298.257223563;
	NAV_E2 = (2-flattening)*flattening; % also e^2
        rad2deg = 180/pi;

	if ((xyz(1) == 0.0) & (xyz(2) == 0.0))
		wlon = 0.0;
	else
		wlon = atan2(xyz(2), xyz(1))*rad2deg;
        end

	if ((xyz(1) == 0.0) & (xyz(2) == 0.0) & (xyz(3) == 0.0))
	        error('WGS xyz at center of earth');
	else 
		% Make initial lat and alt guesses based on spherical earth.
		rhosqrd = xyz(1)*xyz(1) + xyz(2)*xyz(2);
		rho = sqrt(rhosqrd);
		templat = atan2(xyz(3), rho);
		tempalt = sqrt(rhosqrd + xyz(3)*xyz(3)) - A_EARTH;
		rhoerror = 1000.0;
		zerror   = 1000.0;

		%  Newton's method iteration on templat and tempalt makes
		%	the residuals on rho and z progressively smaller.  Loop
		%	is implemented as a 'while' instead of a 'do' to simplify
		%	porting to MATLAB

		while ((abs(rhoerror) > 1e-6) | (abs(zerror) > 1e-6)) 
			slat = sin(templat);
			clat = cos(templat);
			q = 1 - NAV_E2*slat*slat;
			r_n = A_EARTH/sqrt(q);
			drdl = r_n*NAV_E2*slat*clat/q; % d(r_n)/d(latitutde) 

			rhoerror = (r_n + tempalt)*clat - rho;
			zerror   = (r_n*(1 - NAV_E2) + tempalt)*slat - xyz(3);

			%			        --                               -- --      --
			%			        |  drhoerror/dlat  drhoerror/dalt | |  a  b  |
            % Find Jacobian     |		                          |=|        |
			%			        |   dzerror/dlat    dzerror/dalt  | |  c  d  |
			%			        --                               -- --      -- 

			aa = drdl*clat - (r_n + tempalt)*slat;
			bb = clat;
			cc = (1 - NAV_E2)*(drdl*slat + r_n*clat);
			dd = slat;

			%  Apply correction = inv(Jacobian)*errorvector

			invdet = 1.0/(aa*dd - bb*cc);
			templat = templat - invdet*(+dd*rhoerror -bb*zerror);
			tempalt = tempalt - invdet*(-cc*rhoerror +aa*zerror);
		end

		wlat = templat*rad2deg;
		walt = tempalt;
	end

    lla = [wlat, wlon, walt];