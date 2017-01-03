%
% Compares tightly-coupled integrated GPS/INS navigation using pseudoranges
% with GPS-only navigation and INS-only navigation.
%
% This could be like comparing apples and oranges, because the GPS-only
% simulation requires a vehicle tracking filter, whereas the INS-only and
% integrated GPS/INS filters rely on an INS error propagation model that
% is unrelated.
%
% To make the results more comparable, all uncertainties begin after 100
% seconds of position and velocity filtering for a  stationary vehicle,
% before simulation begins.  The
%
% Propagates covariance matrices of estimation uncertainty for each problem
% Simulates GPS satellite geometries to get pseudorange geometries.
%
% Uses the following stochastic system models:
%
% 1. 29-state model for exponentially correlated pseudorange errors, used 
%    for GPS-only navigation and tightly-coupled GPS/INS navigation.
%
% 2. 9-state model for vehicle position, velocity and acceleration, used
%    for GPS-only navigation with pseudoranges.
%
% 3. 8-state INS navigation error model with a parameter characterizing
%    CEP rate.  Used for INS-only navigation and tightly-coupled GPS/INS
%    navigation with pseudoranges.
%
clear all;
close all;
%
% Load satellite ephemerides and initialize 29-state pseudorange model
%
global RA PA
YUMAdata;         % 08 March 2006 almanac data with 29 satellites
NoSats  = 29;     % Number Of Satellites
dt      = 1;      % time interval between GPS pseudoranges
s15     = 0.25881904510252; % sine of 15 degrees (for selecting Sats)
sigmap0 = 20*sqrt(3);   % initial RMS position uncertainty (radial)
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
RPR     = sigmaMN^2*eye(NoSats); % Pseudorange measurement noise covariance
%
% Earth environment
%
Lat     = 40*pi/180;        % Latitude of antenna location
Lon     = 0;                % Longitude of location
Alt     = 100;              % Altitude of location
g               = 9.8;      % gravitational acceleration [m/s/s]
OmegaEarth      = 7.3e-5;   % earthrate [rad/sec]
OmegaSchuler    = 0.00124;  % Schuler frequency [rad/sec
%
% Initialize GPS-only navigation navigation uncertainties for GPS-only
% and GPS/INS navigation scenarios.  Loaded matrix is 38x38 covariance
% matrix of 9-state vehicle model + 29-state satellite model after 100
% seconds of estimation (StartTime = 100);
%
load 'PGPSinit'; % load initial value of 38x38 covariance matris of
                 % estimation uncertainty (PGPS0)
P  = PGPS0;
%
% Initial uncertainties (after 100 s GPS tracking of parked vehicle)
%
% RMS horizontal position uncertainty = 0.93599 [m]
% RMS vertical position uncertainty   = 0.63311 [m]
% RMS horizontal velocity uncertainty = 0.0098948 [m/s]
% RMS vertical velocity uncertainty   = 0.0069966 [m/s]
%
StartTime   = 0;
dt          = 1;         % discrete time step
%
% Vehicle dynamic model parameters based on figure-8 track model
%
RMSPosN = 212.9304;  % meter
RMSPosE = 70.9768;   % meter
RMSPosD = 3.5361;    % meter
RMSVelN = 22.3017;   % m/s
RMSVelE = 14.8678;   % m/s
RMSVelD = 0.37024;   % m/s
RMSAccN = 2.335;     % m/s/s
RMSAccE = 3.1134;    % m/s/s
RMSAccD = 0.038778;  % m/s/s
TauAccN = 13.4097;   % sec
TauAccE = 7.6696;    % sec
TauAccD = 9.6786;    % sec
tauPR   = 60;        % correlation time-constant of pseudorange errors
phiPR   = exp(-dt/tauPR);
PhiPR   = phiPR*eye(NoSats); % pseudorange error S.T.M.
sigmaPR = 10;     % RMS pseudorange uncertainty (steady-state)
qPR     = sigmaPR^2*(1 - exp(-2*dt/tauPR));
QPR     = qPR*eye(NoSats); % covariance of pseudorange disturbance noise
%
% Matching DAMP3 vehicle tracking model parameters used in receiver
%
[PhiN,QN,PN] = DAMP3Params(RMSAccN,RMSVelN,RMSPosN,TauAccN,dt);
[PhiE,QE,PE] = DAMP3Params(RMSAccE,RMSVelE,RMSPosE,TauAccE,dt);
[PhiD,QD,PD] = DAMP3Params(RMSAccD,RMSVelD,RMSPosD,TauAccD,dt);
Phi    = [PhiN,zeros(3,NoSats+6);zeros(3),PhiE,zeros(3,NoSats+3);
           zeros(3,6),PhiD,zeros(3,NoSats);zeros(NoSats,9),PhiPR];
Q      = [QN,zeros(3,NoSats+6);
           zeros(3),QE,zeros(3,NoSats+3);
           zeros(3,6),QD,zeros(3,NoSats);
           zeros(NoSats,9),QPR];
%
% Evaluate GPS-only navigation for 2 hours
%
StartTime = 100;
k = 0;
for t=StartTime:dt:7200+StartTime,
    %
    % Core GPS pseudorange measurement sensitivities for all satellites,
    % visible or not.
    %
    HGPS = HSatSim(t,Lat,Lon,Alt);
    NoSatsAvail = NoSats;
    %
    % Zero out the sensitivities to satellites that are not 15 degrees
    % above the horizon.
    %
    NoSatsAvail = NoSats;
    HPR         = eye(NoSats);
    for m=1:29,
        if HGPS(m,3) > -0.25881904510252 
            NoSatsAvail = NoSatsAvail - 1;
            HGPS(m,1) = 0;
            HGPS(m,2) = 0;
            HGPS(m,3) = 0;
            HPR(m,m)  = 0;
        end;
    end;
    %
    % Measurement sensitivity matrices for DAMP3 tracking filter
    %
    H               = [HGPS(:,1),zeros(NoSats,2),HGPS(:,2),zeros(NoSats,2),HGPS(:,3),zeros(NoSats,2),HPR];
    HP              = H*P;
    K               = HP'/(HP*H' + RPR);
    P               = P - K*HP;
    P               = (P+P')/2;
    k               = k + 1;
    Hr(k)           = (t-StartTime)/3600;
    RMSGPSposH(k)   = sqrt(P(1,1)+P(4,4)); % RMS GPS horiz. pos. error
    RMSGPSposV(k)   = sqrt(P(7,7));        % RMS GPS vertical pos. error
    RMSGPSvelH(k)   = sqrt(P(2,2)+P(5,5)); % RMS GPS horiz. pos. error
    RMSGPSvelV(k)   = sqrt(P(8,8));        % RMS GPS vertical pos. error
    %
    % DAMP3 filter: temporal update
    %
    P   = Phi*P*Phi' + Q;
    P   = (P+P')/2;
end;
%
% Initial value of 8-state navigation error model
%
P8 = zeros(8);
P8(1,1) = PGPS0(1,1);% RMS horizontal pos. uncertainty = 0.93599 [m]
P8(2,2) = PGPS0(4,4);
P8(3,3) = PGPS0(2,2);% RMS horizontal vel. uncertainty = 0.0098948 [m/s]
P8(4,4) = PGPS0(5,5);
P8(5,5) = 1e-12;     % RMS tilt after leveling = 1 microradian 
P8(6,6) = 1e-12;
P8(7,7) = PGPS0(7,7);% RMS vertical pos. uncertainty   = 0.63311 [m]
P8(8,8) = PGPS0(8,8);% RMS vertical vel. uncertainty   = 0.0069966 [m/s]
%
% Initial value of integrated system covariance matrix.
%
PINT0 = [P8,zeros(8,29);zeros(29,8),PGPS0(10:38,10:38)];
%
% Dynamic coefficient matrix (FINS) and state transition matrix (PhiINS)
% for the 6-state horizontal error sub-model of the INS error model.
%
FINS   = [0 0 1 0 0 0; 0 0 0 1 0 0; -OmegaSchuler^2 0 0 -2*sin(Lat)*OmegaEarth -g 0; 0 -OmegaSchuler^2 2*sin(Lat)*OmegaEarth 0 0 g; 0 0 0 0 0 0; 0 0 0 0 0 0];
PhiINS = expm(dt*FINS);
TSchuler    = 5046;    % Schuler period [sec]
%
% Add vertical channel model from m-file VertChanErr.m
% (vertical channel stabilization using altimeter)
% to the horizontal INS error model.
%
PhiVC       = .5*exp(dt/TSchuler)*[1,TSchuler;1/TSchuler,1] + .5*exp(-dt/TSchuler)*[1,-TSchuler;-1/TSchuler,1];
PhiINS      = [PhiINS,zeros(6,2);zeros(2,6),PhiVC];
PhiINT      = [PhiINS,zeros(8,29);zeros(29,8),PhiPR];
%
% Solve for CEP rate as a function of dynamic disturbance noise covariance
% in the 8-state model for INS errors.  This is done by varying q, the 
% variance of dynamic disturbance noise, computing CEP over 1 hour for
% each q-value, and least-squares fitting CEP as a linear function of time
% for each q-value.  The slope of the line is the inferred CEP rate in
% nautical miles per hour.  The tabulated values of CEP rate versus q will
% then be used in a table look-up function to compute q for specified 
% values of CEP rate.
%
j = 0;
clear('Hr');
for log10q=-24:2,
    j       = j + 1;
    Log10q(j) = log10q;
    q(j)    = 10^log10q;
    Q       = zeros(8);
    Q(5,5)  = q(j);
    Q(6,6)  = q(j);
    P       = zeros(8);
    k       = 0;
    for t=0:dt:3600;
        k           = k + 1;
        Hr(k)       = t/3600;
        NMiCEP(j,k) = 1.177410022*sqrt(P(1,1)+P(2,2))/1852;
        %
        % There are 1852 meters per nautical mile.
        % The factor 1.177410022 for converting radial RMS to CEP
        % is exact for circular normal distributions.
        %
        P           = PhiINS*P*PhiINS' + Q;
    end;
end;
%
% CEP rate computed from least-squares straight-line fit of CEP versus
% time over 1 hour period.
%
CEPrate = Hr*NMiCEP'/(Hr*Hr'); % least squares solution for CEP rate
%
% Compute & display q-values for CEP rate= .01, .1, 1, 10, 100, 1000 NMi/Hr
%
j = 0;
for log10CEPrateX=-2:3,
    j = j + 1;
    CEPrateX(j) = 10^log10CEPrateX;
    qX(j)       = 10^interp1(log10(CEPrate),log10(q),log10CEPrateX);
end;
%
% Perform covariance analysis of expected integrated GPS/INS performance
% for INS CEP rates of 0.1, 1. 10, 100 NMi/Hr.  Results will be RMS
% position uncertainties versus time.
%
RINT = 100*eye(29);
for j=1:4;
    INSCEPrate = CEPrateX(j+1);
    INSq       = qX(j+1);
    PPR        = PGPS0(10:38,10:38);  % Initialize pseudorange covariances
    %
    % Dynamic disturbance noise covariance for INS error model for the
    % INS CEP rate and vertical acelerometer noise being simulated
    % 
    % CEP
    %
    QINS       = zeros(8);
    QINS(5,5)  = INSq;
    QINS(6,6)  = INSq;
    mpsprh     = 10^(j-4); % meter/sec per root hour accelerometer noise
    disp(['INS CEP Rate = ',num2str(INSCEPrate),' nmph, INS q = ',num2str(INSq),', RMS Vert. Accelerometer Noise = ',num2str(mpsprh),' [m/s/root-hr]']);
    Qaccelerometer = mpsprh^2*dt/3600;% meter^2/sec^2 per update interval
    QINS(8,8)  = Qaccelerometer;
    QINT       = [QINS,zeros(8,29);zeros(29,8),QPR];
    PINS       = P8; % initialize INS error model uncertainty
    PINT       = PINT0;
    k          = 0;
    for t=StartTime:7200+StartTime;
        %
        % Core GPS pseudorange measurement sensitivities for all satellites,
        % visible or not.
        %
        HGPS = HSatSim(t,Lat,Lon,Alt);
        NoSatsAvail = NoSats;
        %
        % Zero out the sensitivities to satellites that are not 15 degrees
        % above the horizon.
        %
        NoSatsAvail = NoSats;
        HPR         = eye(NoSats);
        for m=1:29,
            if HGPS(m,3) > -0.25881904510252 
                NoSatsAvail = NoSatsAvail - 1;
                HGPS(m,1) = 0;
                HGPS(m,2) = 0;
                HGPS(m,3) = 0;
                HPR(m,m)  = 0;
            end;
        end;
        %
        % Measurement sensitivity matrix for integrating filter
        %
        HINT = [HGPS(:,1:2),zeros(NoSats,4),HGPS(:,3),zeros(NoSats,1),HPR];
        k               = k + 1;
        Hr(k)           = (t-StartTime)/3600;
        RMSINSposH(j,k) = sqrt(PINS(1,1)+PINS(2,2));% RMS INS horiz. error
        RMSINSposV(j,k) = sqrt(PINS(7,7));          % RMS INS vert. error
        RMSINSvelH(j,k) = sqrt(PINS(3,3)+PINS(4,4));% RMS INS horiz. error
        RMSINSvelV(j,k) = sqrt(PINS(8,8));          % RMS INS vert. error
        RMSINStilt(j,k) = sqrt(PINS(5,5)+PINS(6,6));% RMS INS vert. error
        PINS            = PhiINS*PINS*PhiINS' + QINS; % temporal update
        PINS            = (PINS + PINS')/2;
        RMSINTposH(j,k) = sqrt(PINT(1,1)+PINT(2,2));% RMS GPS/INS horiz. error
        RMSINTposV(j,k) = sqrt(PINT(7,7));       % RMS GPS/INS vert. error
        RMSINTvelH(j,k) = sqrt(PINT(3,3)+PINT(4,4));% RMS GPS/INS horiz. error
        RMSINTvelV(j,k) = sqrt(PINT(8,8));       % RMS GPS/INS vert. error
        RMSINTtilt(j,k) = sqrt(PINT(5,5)+PINT(6,6));% RMS GPS/INS tilt
        PINT            = PhiINT*PINT*PhiINT' + QINT;     % temporal update
        PHT             = PINT*HINT';
        K               = PHT/(HINT*PHT+RINT);
        PINT            = PINT - K*PHT';         % observational update
        PINT            = (PINT + PINT')/2;
    end;
end;
%
% Plot up RMS postion uncertainties for GPS-only navigation, INS-only
% navigation, and loosely coupled GPS/INS integration.
%
%
figure;
semilogy(Hr,RMSGPSposH,'k:',Hr,RMSINSposH(1,:),'k--',Hr,RMSINTposH(1,:),'k-');
legend('GPS','INS','GPS/INS');
title('RMS Horizontal Position Uncertainty vs Time');
hold on;
for j=2:4,
    semilogy(Hr,RMSINSposH(j,:),'k--',Hr,RMSINTposH(j,:),'k-');
end;
hold off;
ylabel('RMS Horizontal Position Uncertainty [meter]');
xlabel('Time [hr]');
%
figure;
semilogy(Hr,RMSGPSposV,'k:',Hr,RMSINSposV(1,:),'k--',Hr,RMSINTposV(1,:),'k-');
legend('GPS','INS','GPS/INS');
title('RMS Vertical Position Uncertainty vs Time');
hold on;
for j=2:4,
    semilogy(Hr,RMSINSposV(j,:),'k--',Hr,RMSINTposV(j,:),'k-');
end;
hold off;
ylabel('RMS Vertical Position Uncertainty [meter]');
xlabel('Time [hr]');
%
figure;
semilogy(Hr,RMSGPSvelH,'k:',Hr,RMSINSvelH(1,:),'k--',Hr,RMSINTvelH(1,:),'k-');
legend('GPS','INS','GPS/INS');
title('RMS Horizontal Velocity Uncertainty vs Time');
hold on;
for j=2:4,
    semilogy(Hr,RMSINSposV(j,:),'k--',Hr,RMSINTposV(j,:),'k-');
end;
hold off;
ylabel('RMS Horizontal Velocity Uncertainty [meter]');
xlabel('Time [hr]');
%
figure;
semilogy(Hr,RMSGPSvelV,'k:',Hr,RMSINSvelV(1,:),'k--',Hr,RMSINTvelV(1,:),'k-');
legend('GPS','INS','GPS/INS');
title('RMS Vertical Velocity Uncertainty vs Time');
hold on;
for j=2:4,
    semilogy(Hr,RMSINSvelV(j,:),'k--',Hr,RMSINTvelV(j,:),'k-');
end;
hold off;
ylabel('RMS Vertical Velocity Uncertainty [meter]');
xlabel('Time [hr]');
%
figure;
semilogy(Hr,RMSINStilt(1,:),'k--',Hr,RMSINTtilt(1,:),'k-');
legend('INS','GPS/INS');
title('RMS Tilt Uncertainty vs Time');
hold on;
for j=2:4,
    semilogy(Hr,RMSINStilt(j,:),'k--',Hr,RMSINTtilt(j,:),'k-');
end;
hold off;
ylabel('RMS Tilt Uncertainty [rad]');
xlabel('Time [hr]');