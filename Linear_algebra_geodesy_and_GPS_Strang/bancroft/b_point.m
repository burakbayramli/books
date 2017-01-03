function pos = b_point(pr,sv,time)
%B_POINT Prepares input to the Bancroft algorithm for finding
%    	   a preliminary position of a receiver. The input is
%	      four or more pseudoranges and the coordinates of the
%	      satellites.

%Kai Borre and C.C. Goad 11-24-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $ $Date: 1997/09/26 $

v_light = 299792458;
dtr = pi/180;

m = size(pr,1);
Eph = get_eph('pta.nav');
for t = 1:m
   col_Eph(t) = find_eph(Eph,sv(t),time);
end

pos = zeros(4,1);	      % First guess Earth center
for l = 1:2
   B = [];
   for jsat = 1:m
      k = col_Eph(jsat);
      tx_RAW = time - pr(jsat)/v_light;
      toc = Eph(21,k);
      dt = check_t(tx_RAW-toc);
      tcorr = (Eph(2,k)*dt + Eph(20,k))*dt + Eph(19,k);
      tx_GPS = tx_RAW-tcorr;
      dt = check_t(tx_GPS-toc);
      tcorr = (Eph(2,k)*dt + Eph(20,k))*dt + Eph(19,k);
      tx_GPS = tx_RAW-tcorr;
      X = satpos(tx_GPS, Eph(:,k));
      if l ~= 1
         rhosq = (X(1)-pos(1))^2+(X(2)-pos(2))^2+(X(3)-pos(3))^2;
         traveltime = sqrt(rhosq)/v_light;
         Rot_X = e_r_corr(traveltime,X);
         rhosq = (Rot_X(1)-pos(1))^2 + ...
                      (Rot_X(2)-pos(2))^2 + ...
                              (Rot_X(3)-pos(3))^2;
         traveltime = sqrt(rhosq)/v_light;
         [az,el,dist] = topocent(Rot_X, Rot_X-pos(1:3,:));
         trop = tropo(sin(el*dtr),h/1000,1013.0,293.0,50.0,...
                                    0.0,0.0,0.0);
      else trop = 0;
      end
      corrected_pseudorange = pr(jsat)+v_light*tcorr-trop;
      B(jsat,1) = X(1);
      B(jsat,2) = X(2);
      B(jsat,3) = X(3);
      B(jsat,4) = corrected_pseudorange;
   end; % jsat-loop
   pos = bancroft(B);
   [phi,lambda,h] = togeod(6378137, 298.257223563, ...
                                 pos(1), pos(2), pos(3));
end; % l-loop
clock_offset = pos(4)/v_light;
pos(4) = clock_offset*1.e+9;      % ns
%%%%%%%%%%% b_point.m  %%%%%%%%%%%%%%%%%%%%%%%%%
