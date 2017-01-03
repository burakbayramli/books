function [pos, El, GDOP, basic_obs] = recposRTK(time, good, obs)
% RECPOSRTK Computation of receiver position from pseudoranges
%           using ordinary least-squares principle
%
%           time is the epoch for observations, good is a row of 
%           PRN values, and obs is a column of corresponding  
%           pseudorange observations 

%Kai Borre 07-05-2006
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 2006/07/05  $

global EPH

v_light = 299792458;
dtr = pi/180;
m = size(obs,1);  % number of svs
El = zeros(m,1);

% preliminary guess for receiver position and receiver clock offset
pos = zeros(4,1);
no_iterations = 10; 
ps_corr = [];
sat_pos = [];

for iter = 1:no_iterations
    A = [];
    omc = []; % observed minus computed observation
    for i = 1:m        
        k = good(i);
        tx_RAW = time - obs(i)/v_light;
        t0c = EPH(21,k);
        dt = check_t(tx_RAW-t0c);
        tcorr = (EPH(2,k)*dt + EPH(20,k))*dt + EPH(19,k);
        tx_GPS = tx_RAW-tcorr;
        dt = check_t(tx_GPS-t0c);
        tcorr = (EPH(2,k)*dt + EPH(20,k))*dt + EPH(19,k);
        tx_GPS = tx_RAW-tcorr;
        X = satposRTK(tx_GPS, k);
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
    end % i
    x = A\omc;
    pos = pos+x;
    if iter == no_iterations, GDOP = sqrt(trace(inv(A'*A))); 
        %% two lines that solve an exercise on computing tdop
        % invm = inv(A'*A);
        % tdop = sqrt(invm(4,4))
    end
end % iter
basic_obs = [sat_pos ps_corr];

%%%%%%%%%%%%%%%%%%%%%  recposRTK.m  %%%%%%%%%%%%%%%%%%%%%
