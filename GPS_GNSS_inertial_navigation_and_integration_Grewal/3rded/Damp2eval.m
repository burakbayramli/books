%
% Evaluations of DAMP2 GPS position tracking filters for 
% a range of RMS accelerations and acceleration correlation times.
%
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
clear all;
close all;
global RA PA
YUMAdata;         % 08 March 2006 almanac data with 29 satellites
NoSats  = 29;     % Number Of Satellites
Dt      = 1;      % time interval between GPS pseudoranges
s15     = 0.25881904510252; % sine of 15 degrees (for selecting Sats)
sigma0  = 20*sqrt(3);   % initial RMS position uncertainty (radial)
sigmav  = 200;    % RMS velocity (about 447 mph RMS speed, radial)
%sigmaa  = .5*9.8; % RMS acceleration (1/2 G radial)
sigmap  = 10;     % RMS pseudorange uncertainty (steady-state)
sigmaMN = 10;     % RMS pseudorange measurement noise (white, zero-mean)
taup    = 60;     % correlation time-constant of pseudorange errors
%taua    = 120;    % correlation time-constant of acceleration
sigmaPR = sigmap*sqrt(1 - exp(-2*Dt/taup))/Dt;
%
% Measurement noise covariance
%
R       = sigmaMN^2*eye(NoSats);
%
% Initial simulation conditions
%
Lat     = 40*pi/180;
Lon     = 0;
Alt     = 100;
k = 0;
for log10Gs=-3:.125:2,
    k            =  k + 1;
    log10accG(k) = log10Gs;
    sigmaGs      = 10^log10Gs;      % RMS acceleration [G]
    sigmaa       = 9.8*sigmaGs;     % RMS acceleration [m/sec/sec]
    ell          = 0;
    for log10Hrs=-4:.125:1,
        ell    = ell + 1;
        log10tauaHr(ell) = log10Hrs;
        tauHrs = 10^log10Hrs;
        taua   = tauHrs*3600;  % acceleration correlation time [sec]
        [tauv,rhovelacc,sigmajerk] = Damp2ParamsC(sigmaa,sigmav,taua,Dt);
        disp(['10^(',num2str(log10accG(k)),') G''s, 10^(',num2str(log10Hrs),') Hrs, tau_a = ',num2str(taua),', tau_v = ',num2str(tauv)]);
        %
        % Initial covariance of uncertainty
        %
        Pee     = [sigma0^2/3*eye(3),zeros(3,6);
                   zeros(3),sigmav^2/3*eye(3),rhovelacc*sigmav*sigmaa*eye(3)/3;
                   zeros(3),rhovelacc*sigmav*sigmaa*eye(3)/3,sigmaa^2/3*eye(3)];
        Pen     = zeros(9,NoSats);
        Pnn     = sigmap^2*eye(NoSats);
        P       = [Pee ,zeros(9,NoSats);zeros(NoSats,9),Pnn ];
        %
        % State transition matrix (constant)
        %
        ev      = exp(-Dt/tauv); % velocity
        ea      = exp(-Dt/taua); % acceleration
        ep      = exp(-Dt/taup); % pseudorange
        Phie    = [eye(3),Dt*eye(3),.5*Dt^2*eye(3);
                   zeros(3),ev*eye(3),Dt*eye(3);
                   zeros(3),zeros(3),ea*eye(3)];
        Phin    = [ep*eye(NoSats)];
        Phi     = [Phie,zeros(9,NoSats);
                   zeros(NoSats,9),Phin];
        %
        % Covariance of dynamic disturbance noise
        %
        Qee     = [zeros(6,9);zeros(3,6),sigmajerk^2*Dt^2/3*eye(3)];
        Qnn     = sigmaPR^2*Dt^2*eye(NoSats);
        Q       = [Qee,zeros(9,NoSats);zeros(NoSats,9),Qnn];
        m       = 0;
        count   = 0;
        SS      = 0;
        NoSecs  = 60*60; % One hour of simulation
        for t=0:NoSecs,
            HGPS = [HSatSim(t,Lat,Lon,Alt),zeros(NoSats,6),eye(29)];
            NoSatsAvail = NoSats;
            %
            % Zero out the sensitivities to satellites below 15 degrees
            %
            for j=1:29,
                if HGPS(j,3) > -0.25881904510252 
                    NoSatsAvail = NoSatsAvail - 1;
                    HGPS(j,1) = 0;
                    HGPS(j,2) = 0;
                    HGPS(j,3) = 0;
                end;
            end;
            %
            % Kalman filter
            %
            HP = HGPS*P;
            P = P - (HP'/(HP*HGPS' + R))*HP;
            P = (P+P')/2;
            P = Phi*P*Phi' + Q;
            P = (P+P')/2;
            if t > 100 % exclude the first 100 seconds to allow settling
                KalSq  = (P(1,1)+P(2,2)+P(3,3))/3;
                SS     = SS + KalSq;
                count  = count + 1;
            end;
        end;
        RMSKal(k,ell)  = sqrt(SS/count);
    end;
end;
figure;
[C,h] = contour(log10tauaHr,log10accG,RMSKal,[5,10,20,30,50,100,200,400],'k-');
clabel(C,h);
ylabel('RMS Acceleration [g]');
xlabel('Acceleration Correlation Time [hr]');
title('Effect of Vehicle Dynamics on RMS Position Error [m] at 200 m/s RMS Velocity');
text(log10(1),-3,'\downarrow','HorizontalAlignment','center','VerticalAlignment','bottom');
text(log10(1),-2.7,'1 hr','HorizontalAlignment','center','VerticalAlignment','bottom');
text(log10(1/60),-3,'\downarrow','HorizontalAlignment','center','VerticalAlignment','bottom');
text(log10(1/60),-2.7,'1 min','HorizontalAlignment','center','VerticalAlignment','bottom');
text(log10(1/60/60),-3,'\downarrow','HorizontalAlignment','center','VerticalAlignment','bottom');
text(log10(1/60/60),-2.7,'1 sec','HorizontalAlignment','center','VerticalAlignment','bottom');