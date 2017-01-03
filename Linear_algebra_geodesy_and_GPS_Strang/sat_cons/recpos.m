%RECPOS Least-squares searching for receiver position.
%	     Given 4 or more pseudoranges and ephemerides.
%       Zoom on the plot to detect the search pattern!
%       Idea to this script originates from Clyde C. Goad

%Kai Borre 04-19-96
%Copyright (c) by Kai Borre
%$Revision 1.0 $  $Date: 1997/09/24  $

vlight = 299792458;	         % vacuum speed of light in m/s
Omegae_dot = 7.292115147e-5;  % rotation rate of the earth in rad/s
dtr = pi/180;
ohio = [31 22932343.11;	      % Data from RINEX file "ohiostat.96o"
        18 21463430.59;	      % at time 96  3 30 15 30  5.00
        28 21438529.96;
        29 20470261.34;
        22 21327488.52];
time = rem(512235005,604800);
Eph = get_eph('rinex_n.dat');
[m,n] = size(ohio);
sv = ohio(:,1);

for t = 1:m
   icol = find_eph(Eph,sv(t),time);
   tx_RAW = time-ohio(t,2)/vlight;
   TOC = Eph(21,icol);
   dt = check_t(tx_RAW-TOC);
   tcorr(t) = (Eph(2,icol)*dt + Eph(20,icol))*dt + Eph(19,icol);
   tx_GPS = tx_RAW-tcorr(t);
   XS(:,t) = satpos(tx_GPS, Eph(:,icol));
   [phi(t),lambda(t),h(t)] = togeod(6378137,298.257223563,...
                                         XS(1,t),XS(2,t),XS(3,t));
end
close all

% Satellite positions are now known in the ECEF system in form of
% Cartesian (X,Y,Z) and geographical (phi,lambda).
% First guess for receiver's position is (phi_old,lambda_old)
phi_old = mean(phi);
lambda_old = mean(lambda);
scale = 900;
acc_p = [];
acc_l = [];
Old_Sum = 10^20;
tic

for iter = 1:8  % You may find a nicer upper bound for "iter"
   scale = scale/10;
   sin_phi0 = sin(phi_old*dtr);
   cos_phi0 = cos(phi_old*dtr);
   ndiv = 8;
   for b = 0:ndiv
      psi = b*scale/ndiv;	   % distance
      sin_psi = sin(psi*dtr);
      cos_psi = cos(psi*dtr);
      for alpha = 0:20:340	   % azimuth
         sin_phi2 = sin_phi0*cos_psi...
                              +cos_phi0*sin_psi*cos(alpha*dtr);
         phi2 = asin(sin_phi2)/dtr;
         if cos(phi2) == 0
            sin_dlambda = 0;
         else
            sin_dlambda = sin(alpha*dtr)*sin_psi/cos(phi2*dtr);
         end;
         dlambda = asin(sin_dlambda)/dtr;
         lambda2 = lambda_old + dlambda;
         [XR(1,1),XR(2,1),XR(3,1)] = frgeod(6378137,...
                              298.257223563, phi2, lambda2, 0);
         for t = 1:m
            cal_one_way(1,t) = norm(XS(:,t)-XR);
            sat_clock(t) = tcorr(t)*vlight;
            omegatau = Omegae_dot*cal_one_way(1,t)/vlight;
            R3 = [ cos(omegatau) sin(omegatau) 0;
                  -sin(omegatau) cos(omegatau) 0;
                          0		     0	     1];
            X_ECF(:,t) = R3*XS(:,t);
            cal_one_way(1,t) = norm(X_ECF(:,t)- XR);
            one_way_res(1,t) = ...
                       ohio(t,2)-cal_one_way(1,t)+sat_clock(t);
         end;
         resid_t = one_way_res(1,:)-one_way_res(1,1);
         New_Sum = resid_t*resid_t';
         if New_Sum < Old_Sum
            Old_Sum = New_Sum;
            phi_save = phi2;
            lambda_save = lambda2;
            acc_p = [acc_p phi2];
            acc_l = [acc_l lambda2];
         end;
      end %alpha
   end %b
   fprintf('Sum of residuals squared: %6g\n', Old_Sum);
   phi_old = phi_save
   lambda_old = lambda_save
end %iter
toc

figure
hold on
plot(acc_l,acc_p,'ro', acc_l,acc_p,'c-')
grid
zoom
hold off
fprintf('Final value for latitude  %10.6f\n', phi_old);
fprintf('Final value for longitude %9.6f\n', lambda_old);
% "Exact" values are known as
%   phi_old =	40.00196769
%lambda_old = 276.9785555
%%%%%%%% end recpos.m %%%%%%%%%%%%%%%%%%%%%
