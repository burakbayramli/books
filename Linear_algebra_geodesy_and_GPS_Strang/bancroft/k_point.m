function pos = k_point(pos,pr,sv,time)
%K_POINT  Prepares input to the Kalman algorithm for finding
%  	    the final position of a receiver. The inputs are
%	       preliminary station coordinates calculated by a call
%	       of b_point (Bancroft algorithm), pseudoranges, prn's,
%	       and measurement received time.

%Kai Borre and C.C. Goad 11-24-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

v_light = 299792458;	           % vacuum speed of light, m/s
Omegae_dot = 7.2921151467e-5;   % rotation rate of the earth, rad/s
AtA = zeros(4,4);
AtY = zeros(4,1);
m = size(pr,1);
P = 1.e+6*eye(4);
pseudorange_variance = 1;	     % m^2
Eph = get_eph('pta.nav');
for t = 1:m
   col_Eph(t) = find_eph(Eph,sv(t),time);
end

delta_X = zeros(4,1);
for jsat = 1:m
   k = col_Eph(jsat);
   tx_RAW = time-pr(jsat)/v_light;
   toc = Eph(21,k);
   dt = check_t(tx_RAW-toc);
   tcorr = (Eph(2,k)*dt + Eph(20,k))*dt + Eph(19,k);
   tx_GPS = tx_RAW-tcorr;
   X = satpos(tx_GPS, Eph(:,k));
   traveltime = 70.e-3;	      % 70 ms first guess
   for iter = 1:2
      Rot_X = e_r_corr(traveltime,X);
      rho = norm(Rot_X-pos(1:3,:));
      traveltime = rho/v_light;
   end; % iter-loop
   [phi,lambda,h] = togeod(6378137, 298.257223563, ...
                                     pos(1), pos(2), pos(3));
   [az,el,dist] = topocent(Rot_X,Rot_X-pos(1:3,:));
   corrected_pseudorange = pr(jsat)-...
         tropo(sin(el),h/1000,1013.0,293.0,50.0,0.0,0.0,0.0);
   dx = Rot_X(1)-pos(1);
   dy = Rot_X(2)-pos(2);
   dz = Rot_X(3)-pos(3);
   distance = norm([dx dy dz]);	 % sqrt(dx^2+dy^2+dz^2);
   calc_prange = distance + v_light*(pos(4)*1.e-9 - tcorr);
   omc = corrected_pseudorange - calc_prange;
   fprintf('\nomc for satellite %2.0f: %6.2f m\n',sv(jsat),omc)
   H = ones(4,1);
   H(1) = -dx/distance;
   H(2) = -dy/distance;
   H(3) = -dz/distance;
   H(4) = v_light*1.e-9;
   [AtA, AtY] = normals(AtA,AtY,H,omc,pseudorange_variance);
   [delta_X, P] = k_update(delta_X,P,H',omc,pseudorange_variance);
   pdop = sqrt(P(1,1)+P(2,2)+P(3,3));
   fprintf('PDOP: %3.1f\n',pdop)
   fprintf('Change in position [m] %6.2f %6.2f %6.2f %6.2f\n', ...
                       delta_X(1),delta_X(2),delta_X(3),delta_X(4))
   P,  pause(1)
end; % jsat-loop

Bayes_delta = inv(AtA)*AtY;
fprintf('\n\n\nResults from the Bayes Filter\n')
fprintf('\nChange in position [m] %6.2f %6.2f %6.2f %6.2f\n', ...
             Bayes_delta(1),Bayes_delta(2),...
                           Bayes_delta(3),Bayes_delta(4))
pos = pos+delta_X;
[phi,lambda,h] = togeod(6378137, 298.257223563, ...
                                          pos(1), pos(2), pos(3));
sqsum = 0;
for jsat = 1:m
   k = col_Eph(jsat);
   tx_RAW = time-pr(jsat)/v_light;
   toc = Eph(21,k);
   dt = check_t(tx_RAW-toc);
   tcorr = (Eph(2,k)*dt+Eph(20,k))*dt+Eph(19,k);
   tx_GPS = tx_RAW-tcorr;
   X = satpos(tx_GPS, Eph(:,k));
   traveltime = 70.e-3;		 % 70 ms first guess
   for iter = 1:2
      Rot_X = e_r_corr(traveltime,X);
      rho = norm(Rot_X-pos(1:3,:));
      traveltime = rho/v_light;
   end; % iter-loop
   [az,el,dist] = topocent(Rot_X,Rot_X-pos(1:3,:));
   corrected_pseudorange = pr(jsat)-...
            tropo(sin(el),h/1000,1013.0,293.0,50.0,0.0,0.0,0.0);
   dx = Rot_X(1)-pos(1);
   dy = Rot_X(2)-pos(2);
   dz = Rot_X(3)-pos(3);
   distance = norm([dx dy dz]);	 % sqrt(dx^2+dy^2+dz^2);
   calc_prange = distance + v_light*(pos(4)*1.e-9 - tcorr);
   omc = corrected_pseudorange - calc_prange;
   fprintf('omc for satellite %2.0f: %6.2f m\n',sv(jsat),omc)
   sqsum = sqsum + omc^2;
end  % jsat-loop
rms = sqrt(sqsum/m);
fprintf('\nRMS error of absolute position: %6.2f m\n',rms)
%%%%%%%%%%% k_point.m  %%%%%%%%%%%%%%%%%%%%%%%%%
