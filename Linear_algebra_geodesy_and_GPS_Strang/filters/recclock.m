function  offset = recclock(ofile, navfile)
% RECCLOCK  Estimation of receiver clock offset and position
%	         through batch processing. Data are read from
%	         the RINEX ofile.
%	         The processing is iterated three times due to
%           non-linearity in the position determination

% Typical call: recclock('pta.96o', 'pta.nav')

%Kai Borre 03-29-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

% if exist(navfile) == 0,	rinexe('pta.96n', navfile), end
e = exist('recclock.eps');
if e ~= 0
   delete recclock.eps
end

tic
v_light = 299792458;    % vacuum speed of light m/s
fid = fopen(ofile,'rt');
[Obs_types, ant_delta, ifound_types, eof1] = anheader(ofile);
if ((ifound_types == 0) | (eof1 == 1))
   error('Basic information is missing in RINEX file')
end;
NoObs_types = size(Obs_types,2)/2;

% Downloading of ephemeris data
Eph = get_eph(navfile);
j = fobs_typ(Obs_types,'P2');

fid = fopen(ofile,'rt');
[tr_RAW, dt, sv, eof2] = fepoch_0(fid);
NoSv = size(sv,1);
obs = grabdata(fid, NoSv, NoObs_types);
pr = obs(:,j);

% CALL OF BAYES FILTER FOR FIRST POSITION
pos = b_point(pr,sv,tr_RAW,navfile);
fprintf(['\nPreliminary position:\n X = %10.2f Y = %10.2f', ...
                       ' Z = %10.2f\n\n'], pos(1),pos(2),pos(3))

for iteration = 1:3
   fid = fopen(ofile,'rt');
   reduced_normals = zeros(3,3);
   reduced_absolute = zeros(3,1);
   eTe = [];
   eTb = [];
   eTA = [];
   no_epochs = 0;   
   % while eof2 ~= 1
   for tt = 1:80      
      fprintf('tt: %6.0f\n', tt)
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
      A = zeros(NoSv,3);
      omc = zeros(NoSv,1);
      for jsat = 1:NoSv
         k = col_Eph(jsat);
         tx_RAW = tr_RAW - pr(jsat)/v_light;
         Toc = Eph(21,k);
         dt = check_t(tx_RAW - Toc);
         a0 = Eph(19,k);
         a1 = Eph(20,k);
         a2 = Eph(2,k);
         tcorr = a0 + (a1 + a2*dt)*dt;
         tx_GPS = tx_RAW - tcorr;
         X = satpos(tx_GPS, Eph(:,k));
         traveltime = 70.e-3;	  % First guess: 70 ms
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
         distance = norm([dx dy dz]);
         calculated_pseudorange = distance - v_light*tcorr;
         omc(jsat,1) = corrected_pseudorange - calculated_pseudorange;
         A(jsat,1) = -dx/distance;
         A(jsat,2) = -dy/distance;
         A(jsat,3) = -dz/distance;
      end; % jsat-loop
      % fprintf('\n omc  %12.3f', omc(:,1))
      
      % Formation of Normal Equations
      % We have NoSv                    number of sv.s
      %		    b = omc  right side,    dimension NoSv by 1;
      %		    A			                dimension NoSv by 3;
      %		    sum(A)		             dimension 1 by NoSv;
      eTe = [eTe NoSv];
      eTb = [eTb sum(omc)];
      eTA = [eTA sum(A)'];
      reduced_normals = reduced_normals + A'*A-sum(A)'*sum(A)/NoSv;
      reduced_absolute = reduced_absolute + ...
                                    A'*omc - sum(A)'*sum(omc)/NoSv;
      no_epochs = no_epochs +1;
   end % while loop
   x = inv(reduced_normals)*reduced_absolute
   pos(1:3,1) = pos(1:3,1) + x;
end % iteration

fprintf(['\nFinal position:\n X = %10.2f Y = %10.2f', ...
                           ' Z = %10.2f\n\n'], pos(1),pos(2),pos(3))

for epoch = 1:no_epochs
   rec_clk_offset(epoch) =  (eTb(epoch)-eTA(:,epoch)'*x)/ ...
                         (eTe(epoch)*v_light);  % offset in seconds
end
offset = rec_clk_offset*1.e9;

toc
plot(rec_clk_offset*1.e+9)  % in nanoseconds
title('Receiver clock offset as determined by batch processing',...
                                                      'Fontsize',16)
xlabel('Epochs,  interval 15 s','Fontsize',16)
ylabel('Clock offset [ns]','Fontsize',16)
set(gca,'Fontsize',16);
print recclock -deps

fclose('all');
%%%%%%%%% end recclock.m %%%%%%%%%
