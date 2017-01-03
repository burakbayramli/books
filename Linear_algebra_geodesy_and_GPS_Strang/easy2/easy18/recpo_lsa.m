function [pos, El, GDOP, basic_obs] = recpo_lsa(obs,sats,time,Eph)
% RECPO_LS Computation of receiver position from pseudoranges
%          using ordinary least-squares principle

%Kai Borre 31-10-2001
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 2002/07/10  $

% easy3 modified for computing the Hessian matrix 
% of the observation equations; 
% and mean vector for satellite directions
% to be compared with the eigenvalues of inv(A'*A)

% December 11, 2005

global H

v_light = 299792458;
dtr = pi/180;
m = size(obs,1);  % number of svs
el = zeros(m,1);
% identify ephemerides columns in Eph
for t = 1:m
    col_Eph(t) = find_eph(Eph,sats(t),time);
end
% preliminary guess for receiver position and receiver clock offset
pos = zeros(4,1);
no_iterations = 6; 
ps_corr = [];
sat_pos = [];

for iter = 1:no_iterations
    A = [];
    H = [];
    omc = []; % observed minus computed observation
    dir_vector = [];
    
    for i = 1:m        
        k = col_Eph(i);
        tx_RAW = time - obs(i)/v_light;
        t0c = Eph(21,k);
        dt = check_t(tx_RAW-t0c);
        tcorr = (Eph(2,k)*dt + Eph(20,k))*dt + Eph(19,k);
        tx_GPS = tx_RAW-tcorr;
        dt = check_t(tx_GPS-t0c);
        tcorr = (Eph(2,k)*dt + Eph(20,k))*dt + Eph(19,k);
        tx_GPS = tx_RAW-tcorr;
        X = satpos(tx_GPS, Eph(:,k));
        if iter == 1
            traveltime = 0.072;
            Rot_X = X;
            trop = 0;
        else
            rho2 = (X(1)-pos(1))^2+(X(2)-pos(2))^2+(X(3)-pos(3))^2;
            traveltime = sqrt(rho2)/v_light;
            Rot_X = e_r_corr(traveltime,X);
            rho2 = (Rot_X(1)-pos(1))^2+(Rot_X(2)-pos(2))^2+(Rot_X(3)-pos(3))^2;          
            [az,el,dist] = topocent(pos(1:3,:),Rot_X-pos(1:3,:));                                                            
            if iter == no_iterations, El(i) = el; end
            trop = tropo(sin(el*dtr),0.0,1013.0,293.0,50.0,...
                0.0,0.0,0.0);    
        end
        % subtraction of pos(4) corrects for receiver clock offset and
        % v_light*tcorr is the satellite clock offset
        if iter == no_iterations
            ps_corr = [ps_corr; obs(i)+v_light*tcorr-trop];
            sat_pos = [sat_pos; X'];
        end
        omc = [omc; obs(i)-norm(Rot_X-pos(1:3),'fro')-pos(4)+v_light*tcorr-trop]; 
        A = [A; (-(Rot_X(1)-pos(1)))/obs(i)...
                (-(Rot_X(2)-pos(2)))/obs(i) ...
                (-(Rot_X(3)-pos(3)))/obs(i) 1];
                % Compute a unit vector at receiver position pointing to
                % satellite
                unit_vec = (Rot_X(1:3,1)-pos(1:3,1))/norm(Rot_X(1:3,1)-pos(1:3,1));
                dir_vector = [dir_vector unit_vec];
    end % i
    x = A\omc;
    pos = pos+x;
 
    if iter == no_iterations, GDOP = sqrt(trace(inv(A'*A))); 
        %% two lines that solve an exercise on computing tdop
        % invm = inv(A'*A);
        % tdop = sqrt(invm(4,4))
    end
end % iter

%    dir_vector = mean(dir_vector,2)
%    Sigma = inv(A(1:3,1:3)'*A(1:3,1:3))
%   [a,b] = eig(Sigma)
    

basic_obs = [sat_pos ps_corr];

%%%%%%%%%%%%%%%%%%%%%  recpo_lsa.m  %%%%%%%%%%%%%%%%%%%%%
