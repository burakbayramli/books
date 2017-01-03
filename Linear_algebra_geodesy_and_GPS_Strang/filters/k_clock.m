function [pos, Var] = k_clock(pos,pr,sv,time,navfile)
% K_CLOCK  Prepares input to the Kalman algorithm for finding
%	        receiver clock offset. The inputs are receiver
%	        coordinates calculated by a call of b_point
%	        (Bancroft algorithm), pseudoranges, PRN's, and
%	        measurement received time.

%Kai Borre, 03-16-97
%Copyright (c) 1997 by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

v_light = 299792458;	           % vacuum speed of light, m/s
Omegae_dot = 7.2921151467e-5;   % rotation rate of the Earth, rad/s
AtA = zeros(4,4);
Atb = zeros(4,1);
m = size(pr,1);
% P is set such that receiver coordinates do not change;
% however, the receiver clock offset may change
P = zeros(4,4);
P(4,4) = 1.e+10;
pseudorange_variance = 1;	 % unit m^2
Eph = get_eph(navfile);
for t = 1:m
   col_Eph(t) = find_eph(Eph,sv(t),time);
end

A = ones(4,1);
delta_X = zeros(4,1);

for jsat = 1:m
   k = col_Eph(jsat);
   tx_RAW = time-pr(jsat)/v_light;
   toc = Eph(21,k);
   dt = check_t(tx_RAW-toc);
   tcorr = (Eph(2,k)*dt + Eph(20,k))*dt + Eph(19,k);
   tx_GPS = tx_RAW-tcorr;
   X = satpos(tx_GPS, Eph(:,k));
   traveltime = 70.e-3;	      % First guess: 70 ms 
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
   distance = norm([dx dy dz]);
   calc_prange = distance + v_light*(pos(4)*1.e-9 - tcorr);
   % calc_prange = distance + v_light*(- tcorr);
   omc = corrected_pseudorange - calc_prange;
   A(1) = -dx/distance;
   A(2) = -dy/distance;
   A(3) = -dz/distance;
   A(4) = v_light*1.e-9;
   [AtA, Atb] = normals(AtA,Atb,A,omc,pseudorange_variance);
   [delta_X, P] = k_update(delta_X,P,A',omc,pseudorange_variance);
end; % jsat-loop
pos = pos+delta_X;
Var = P(4,4);
%%%%%%%%%%% k_clock.m  %%%%%%%%%%%%%%%%%%%%%%%%%
