%
% Flat earth trajectory simulator using DAMP2/3 tracker model
%
clear all;
close all;
%
% Load satellite ephemerides and initialize GNSS parameters
%
global RA PA
YUMAdata;         % 08 March 2006 almanac data with 29 satellites
NoSats  = 29;     % Number Of Satellites
dt      = 1;      % time interval between GPS pseudoranges
s15     = 0.25881904510252; % sine of 15 degrees (for selecting Sats)
%sigmap0 = 20*sqrt(3);   % initial RMS position uncertainty (radial)
sigmaPR = 10;     % RMS pseudorange uncertainty (steady-state)
sigmaMN = 10;     % RMS pseudorange measurement noise (white, zero-mean)
tauPR   = 60;     % correlation time-constant of pseudorange errors
P0PR    = sigmaPR^2*eye(NoSats);
phiPR   = exp(-dt/tauPR);
PhiPR   = phiPR*eye(NoSats); % pseudorange error S.T.M.
qPR     = sigmaPR^2*(1 - exp(-2*dt/tauPR));
QPR     = qPR*eye(NoSats); % covariance of pseudorange disturbance noise
sqrtqPR = sqrt(qPR);       % std. dev. of pseudorange disturbance noise
xPR     = sigmaPR*randn(NoSats,1); % initial state of pseudorange errors
%
% Measurement noise covariance
%
RPR     = sigmaMN^2*eye(NoSats);
%
% Initial simulation conditions
%
Lat     = 40*pi/180;
Lon     = 0;
Alt     = 1000;
%
% Initialize satellite signal delay error estimates at true values
%
xbar    = [zeros(7,1);xPR];
xhat    = [zeros(7,1);xPR];
%
% Vehicle dynamic model parameters
%
dt      = 1;    % discrete time step
RMSPosD = 50;  % meter (from profiled altitude)
RMSVelN = 10;   % m/s
RMSVelE = 10;   % m/s
RMSVelD = 1;   % m/s
RMSAccN = 1;    % m/s/s
RMSAccE = 1;    % m/s/s
RMSAccD = 1;    % m/s/s
TauAccN = 180;  % sec
TauAccE = 180;  % sec
TauAccD = 600;  % sec
%
% DAMP2 vehicle tracking model parameters for horizontal motion
%
[Phi2N,Q2N,P2N] = DAMP2Params(RMSAccN,RMSVelN,TauAccN,dt);
[Phi2E,Q2E,P2E] = DAMP2Params(RMSAccE,RMSVelE,TauAccE,dt);
%
% DAMP3 vehicle tracking model parameters for vertical motion
%
[Phi3D,Q3D,P3D] = DAMP3Params(RMSAccD,RMSVelD,RMSPosD,TauAccD,dt);
%
% Model parameters (same for simulator and estimator)
%
Phi    = [Phi2N,zeros(2,NoSats+5);zeros(2),Phi2E,zeros(2,NoSats+3);
        zeros(3,4),Phi3D,zeros(3,NoSats);zeros(NoSats,7),PhiPR];
Q      = [Q2N,zeros(2,NoSats+5);
        zeros(2),Q2E,zeros(2,NoSats+3);
        zeros(3,4),Q3D,zeros(3,NoSats);
        zeros(NoSats,7),QPR];
sqrtQ  = sqrt(Q);
P      = [P2N,zeros(2,NoSats+5);
        zeros(2),P2E,zeros(2,NoSats+3);
        zeros(3,4),P3D,zeros(3,NoSats);
        zeros(NoSats,7),P0PR];
%
% Simulated vehicle on figure-8 track and 9--11 satellites in view
%
k         = 0;
StartTime = 3600; % Allow 1 hour settling before sampling data
Hours     = 2;    % Number of simulation hours after StartTime
                  % An integer multiple of 1/1800.
N         = round(Hours*3600+StartTime+1);
M         = (N - 1)/2;
Alt       = 1000*(1-cos((0:N-1)/N*2*pi);
xtrue     = [];
xest      = [];
time      = [];
for t=0:dt:Hours*3600+StartTime,
    %
    % Initialize estimate with true values
    %
    if t==0
        xhat   = xbar;
    end;
    %
    % Core GPS pseudorange measurement sensitivities for all satellites,
    % visible or not.
    %
    HGPS        = HSatSim(t,Lat,Lon,Alt);
    HPR         = eye(NoSats);
    %
    % Zero out the sensitivities to satellites that are not 15 degrees
    % or more above the horizon.
    %
    NoSatsAvail = NoSats;
    for j=1:29,
        if HGPS(j,3) > -0.25881904510252 
            NoSatsAvail = NoSatsAvail - 1;
            HGPS(j,1) = 0;
            HGPS(j,2) = 0;
            HGPS(j,3) = 0;
            HPR(j,j)  = 0;
        end;
    end;
    %
    % Measurement sensitivity matrix
    %
    H  = [HGPS(:,1),zeros(NoSats,1),HGPS(:,2),zeros(NoSats,1),HGPS(:,3),zeros(NoSats,2),HPR];
    %
    % Measured pseudorange variation due to antenna location and unknown
    % satellite signal delays.
    %
    z   = H*xbar + sigmaMN*randn(NoSats,1);
    %
    % DAMP2/3 tracker filter: observational update
    %
    HP      = H*P;
    K       = HP'/(HP*H' + RPR);
    xhat    = xhat + K*(z - H*xhat);
    P       = P - K*HP;
    P       = (P+P')/2;
    if t >= StartTime % exclude the first 100 seconds to allow settling
        k = k + 1;
        xtrue    = [xtrue,xbar];
        xest     = [xest,xhat];
        time     = [time,t];
    end;
    %
    % Temporal update
    %
    xhat = Phi*xhat;
    P    = Phi*P*Phi' + Q;
    P    = (P+P')/2;
    xbar = Phi*xbar + sqrtQ*randn(NoSats+7,1);
end;
figure;
plot(xtrue(3,:),xtrue(1,:),'b-',xest(3,:),xest(1,:),'r-');
axis equal;
title('Simulated (blue) and estimated (red) horizontal trajectories');
xlabel('Easting [m]');
ylabel('Nothing [m]');
figure;
plot(time/3600,xtrue(5,:)+Alt,'b-',time/3600,xest(5,:)+Alt,'r-');
title('Simulated (blue) and estimated (red) altitude');
xlabel('Time [hr]');
ylabel('Altitude [m]');


