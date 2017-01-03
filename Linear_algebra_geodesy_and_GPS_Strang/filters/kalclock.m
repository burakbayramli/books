function offset = kalclock(ofile, navfile, extended_filter)
% KALCLOCK   Estimates receiver clock offset and position as
%     	    read from the RINEX o-file. A RINEX navigation
%	          nav-file is also needed.
%	          Extended filter is used, if extended_filter = 1

%Typical call: kalclock('pta.96o', 'pta.96n', 1)

%Kai Borre 03-29-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

%rinexe(enavfile,navfile)
v_light = 299792458;         % vacuum speed of light m/s
pseudorange_variance = 1;

tic
fid = fopen(ofile,'rt');
[Obs_types, ant_delta, ifound_types, eof1] = anheader(ofile);
if ((ifound_types == 0) | (eof1 == 1))
   error('Basic information is missing in RINEX file')
end;
NoObs_types = size(Obs_types,2)/2;

% Downloading of ephemeris data
Eph = get_eph(navfile);
max_epochs = 80;		         % we use 80 epochs of data
j = fobs_typ(Obs_types,'P2');
fid = fopen(ofile,'rt');

[tr_RAW, dt, sv, eof2] = fepoch_0(fid);
NoSv = size(sv,1);
obs = grabdata(fid, NoSv, NoObs_types);
pr = obs(:,j);

% CALL OF BAYES FILTER FOR FIRST POSITION
pos = b_point(pr,sv,tr_RAW,navfile);
fprintf(['\nPreliminary position:\n X = %10.2f Y = %10.2f', ...
      ' Z = %10.2f T = %10.2f\n\n'], pos(1),pos(2),pos(3),pos(4))

max_iter = 3;
if extended_filter == 1
   max_iter = 1; 
end

for iteration = 1:max_iter
   P = zeros(4,4);
   Big =10^10;
   P(1,1) = Big;
   P(2,2) = Big;
   P(3,3) = Big;
   q = 1.e-6;
   x = zeros(4,1);
   x(4,1) = pos(4);
   %   rec_clk_offset = [];
   rec_clk_offset = zeros(1,max_epochs);
   fid = fopen(ofile,'rt');
   for iepoch = 1:max_epochs    % loop over epochs
      P(4,4) = P(4,4) + q;
      [tr_RAW, dt, sv, eof2] = fepoch_0(fid);
      if (eof2 == 1)
         break;
      end
      NoSv = size(sv,1);
      for t = 1:NoSv
         col_Eph(t) = find_eph(Eph,sv(t),tr_RAW);
      end
      obs = grabdata(fid, NoSv, NoObs_types);
      pr = obs(:,j);
      % Formation of Observation Equations
      for jsat = 1:NoSv
         k = col_Eph(jsat);
         tx_RAW = tr_RAW - pr(jsat)/v_light;
         Toc = Eph(21,k);
         dt = check_t(tx_RAW - Toc);
         a0 = Eph(19,k);
         a1 = Eph(20,k);
         a2 = Eph(2,k);
         sat_clk_corr = a0 + (a1 + a2*dt)*dt;
         tx_GPS = tx_RAW - sat_clk_corr;
         X = satpos(tx_GPS, Eph(:,k));
         traveltime = 70.e-3;	    % First guess: 70 ms 
         for iter = 1:2
            Rot_X = e_r_corr(traveltime, X);
            rho = norm(Rot_X - pos(1:3,1));
            traveltime = rho/v_light;
         end; % iter-loop
         [phi,lambda,h] = togeod(6378137, 298.257223563, ...
                                  pos(1,1), pos(2,1), pos(3,1));
         [az,el,dist] = topocent(Rot_X, Rot_X-pos(1:3,1));
         corrected_pseudorange = pr(jsat) - ...
            tropo(sin(el),h/1000,1013.0,293.0,50.0,0.0,0.0,0.0);
         dx = Rot_X(1) - pos(1,1);
         dy = Rot_X(2) - pos(2,1);
         dz = Rot_X(3) - pos(3,1);
         distance = norm([dx dy dz]);   % sqrt(dx^2+dy^2+dz^2);
         calculated_pseudorange = distance - v_light*sat_clk_corr;
         Y = corrected_pseudorange - calculated_pseudorange;
         H(1,1) = -dx/distance;
         H(2,1) = -dy/distance;
         H(3,1) = -dz/distance;
         H(4,1) = v_light;
         [x,P] = k_update(x,P,H',Y,pseudorange_variance);
      end; % jsat-loop
      if extended_filter == 1
         pos(1:3,1) = pos(1:3,1)+x(1:3,1);
         x(1:3,1)= [0; 0; 0];
      end
      %rec_clk_offset = [rec_clk_offset x(4,1)];
      rec_clk_offset(1,iepoch) = x(4,1);
   end % iepoch-loop
   fprintf('\n Solution: %6.2f %6.2f %6.2f %6.2f\n',...
                                      x(1), x(2), x(3), x(4)*1.e9)
   pos(1:3,1) = pos(1:3,1)+x(1:3,1)
end % iteration
offset = rec_clk_offset*1.e9;
toc

plot(rec_clk_offset*1.e+9)  % in nanoseconds
title('Receiver clock offset as determined by Kalman filter', ...
                                                   'Fontsize', 16)
xlabel('Epochs,  interval 15 s', 'Fontsize', 16)
ylabel('Clock offset [ns]', 'Fontsize', 16)
set(gca, 'Fontsize', 16)
print kalclock -deps

fclose('all');
%%%%%%%%% end kalclock.m %%%%%%%%%
