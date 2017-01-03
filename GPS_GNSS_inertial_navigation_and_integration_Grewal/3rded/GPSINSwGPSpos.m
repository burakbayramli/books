%
% A single-parameter model of horizontal INS errors is used to characterize
% CEP rate as a function of that parameter, then to characterize GPS/INS
% performance over 1 hour as a function of INS CEP rate for a loosely-
% coupled integration using GPS positon outputs only.
% 
%
clear all;
close all;
%
dt              = 1;        % time-sep
g               = 9.8;      % gravitational acceleration [m/s/s]
OmegaEarth      = 7.3e-5;   % earthrate [rad/sec]
OmegaSchuler    = 0.00124;  % Schuler frequency [rad/sec]
phi             = 40*pi/180;% latitude [rad]
%
% Dynamic coefficient matrix (FINS) and state transition matrix (PhiINS)
% for the 6-state INS horizontal error model.
%
FINS   = [0 0 1 0 0 0; 0 0 0 1 0 0; -OmegaSchuler^2 0 0 -2*sin(phi)*OmegaEarth -g 0; 0 -OmegaSchuler^2 2*sin(phi)*OmegaEarth 0 0 g; 0 0 0 0 0 0; 0 0 0 0 0 0];
PhiINS = expm(dt*FINS);
%
% Compute CEP rate as a function of the model parameter q
%
j = 0;
for log10q=-24:2,
    j       = j + 1;
    Log10q(j) = log10q;
    q(j)    = 10^log10q;
    Q       = zeros(6);
    Q(5,5)  = q(j);
    Q(6,6)  = q(j);
    P       = zeros(6);
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
% CEP rate is computed from least-squares straight-line fit of CEP versus
% time over 1 hour period.
%
CEPrate = Hr*NMiCEP'/(Hr*Hr'); % least squares solution for CEP rate
%
% Plot CEP rate versus q
%
loglog(q,CEPrate,'k-');
xlabel('q-parameter [rad^2/sec]');
ylabel('CEP Rate [NMi/Hr]');
title('Single-parameter Model for INS Performance');
%
% Compute & display q-values for CEP rate= .01, .1, 1, 10, 100, 1000 NMi/Hr
%
j = 0;
for log10CEPrateX=-2:3,
    j = j + 1;
    CEPrateX(j) = 10^log10CEPrateX;
    qX(j)       = 10^interp1(log10(CEPrate),log10(q),log10CEPrateX);
    disp(['CEP Rate = ',num2str(CEPrateX(j)),'[NMi/Hr], q = ',num2str(qX(j))]);
end;
%
% Perform covariance analysis of expected integrated GPS/INS performance
% for INS CEP rates of 0.1, 1. 10, 100 NMi/Hr.  Results will be RMS
% position uncertainties versus time.
%
TSchuler    = 5046;    % Schuler period [sec]
%
% Vertical channel model from m-file VertChanErr.m
% (vertical channel stabilization using altimeter)
%
PhiVC       = .5*exp(dt/TSchuler)*[1,TSchuler;1/TSchuler,1] + .5*exp(-dt/TSchuler)*[1,-TSchuler;-1/TSchuler,1];
PhiINS      = [PhiINS,zeros(6,2);zeros(2,6),PhiVC];
%
% Trajectory statistical parameters for DAMP3 tracking model
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
%
% DAMP3 vehicle tracking model parameters
%
[Phi3N,Q3N,P3N] = DAMP3Params(RMSAccN,RMSVelN,RMSPosN,TauAccN,dt);
[Phi3E,Q3E,P3E] = DAMP3Params(RMSAccE,RMSVelE,RMSPosE,TauAccE,dt);
[Phi3D,Q3D,P3D] = DAMP3Params(RMSAccD,RMSVelD,RMSPosD,TauAccD,dt);
Phi3    = [Phi3N,zeros(3,6);zeros(3),Phi3E,zeros(3);zeros(3,6),Phi3D];
Q3      = [Q3N,zeros(3,6);
    zeros(3),Q3E,zeros(3);
    zeros(3,6),Q3D];
P3      = [P3N,zeros(3,6);
    zeros(3),P3E,zeros(3);
    zeros(3,6),P3D];
%
% Parameters for GPS receiver error model
%
RMSGPSN = 10.0715;
RMSGPSE = 9.1029;
RMSGPSD = 1.5347;
tauGPS = 39;
PhiGPS = exp(-dt/tauGPS)*eye(3);
PGPS   = zeros(3);
PGPS(1,1) = RMSGPSN^2;
PGPS(2,2) = RMSGPSE^2;
PGPS(3,3) = RMSGPSD^2;
QGPS      = PGPS - PhiGPS*PGPS*PhiGPS';
RMSGPShor = sqrt(RMSGPSN^2+RMSGPSE^2);
RMSGPSvert= RMSGPSD;
%
% Measurement sensitivity matrix for GPS/INS integration
% Measured values are 3 position components from GPS receiver, plus
% 3 position components from INS.
%
H   = [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0;
       0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0;
       0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0;
       1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0;
       0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0;
       0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0];
Phi = [Phi3,zeros(9,11);zeros(3,9),PhiGPS,zeros(3,8);zeros(8,12),PhiINS];
R   = 1e-2*eye(6); % assume 0.1 m RMS white noise
%
%
%
for j=1:4;
    INSCEPrate = CEPrateX(j+1);
    INSq       = qX(j+1);
    disp(['INS CEP Rate = ',num2str(INSCEPrate),' nmph, INS q = ',num2str(INSq)]);
    QINS       = zeros(8);
    QINS(5,5)  = INSq;
    QINS(6,6)  = INSq;
    mpsprh     = 10^(j-4);
    Qaccelerometer = mpsprh^2*dt/3600;
    QINS(8,8)  = Qaccelerometer;
    Q   = [Q3,zeros(9,11);zeros(3,9),QGPS,zeros(3,8);zeros(8,12),QINS];
    PINS       = zeros(8); % initialize INS with zero error
    PINS(7,7)  = 1;        % except 1 meter RMS in vertical channel
    P          = [P3,zeros(9,11);zeros(3,9),PGPS,zeros(3,8);zeros(8,12),PINS];
    k          = 0;
    for t=0:7200;
        k               = k + 1;
        Hr(k)           = t/3600;
        RMSGPSH(k)      = RMSGPShor;    % RMS GPS horizontal error
        RMSGPSV(k)      = RMSGPSvert;   % RMS GPS vertical error
        RMSINSH(j,k)    = sqrt(PINS(1,1)+PINS(2,2));% RMS INS horiz. error
        RMSINSV(j,k)    = sqrt(PINS(7,7));          % RMS INS vert. error
        PINS            = PhiINS*PINS*PhiINS' + QINS; % temporal update
        PINS            = (PINS + PINS')/2;
        RMSINTH(j,k)    = sqrt(P(1,1)+P(4,4));% RMS GPS/INS horiz. error
        RMSINTV(j,k)    = sqrt(P(7,7));       % RMS GPS/INS vert. error
        P               = Phi*P*Phi' + Q;     % temporal update
        PHT             = P*H';
        K               = PHT/(H*PHT+R);
        P               = P - K*PHT';         % observational update
        P               = (P + P')/2;
    end;
end;
%
% Plot up RMS postion uncertainties for GPS-only navigation, INS-only
% navigation, and loosely coupled GPS/INS integration.
%
figure;
subplot(2,1,1),
semilogy(Hr,RMSGPSH,'k:',Hr,RMSINSH(1,:),'k--',Hr,RMSINTH(1,:),'k-');
legend('GPS','INS','GPS/INS');
title('Loosely Coupled GPS/INS Integration Performances');
hold on;
for j=2:4,
    semilogy(Hr,RMSINSH(j,:),'k--',Hr,RMSINTH(j,:),'k-');
end;
ylabel('RMS Horiz. Err. [m]');
subplot(2,1,2),
semilogy(Hr,RMSGPSV,'k:',Hr,RMSINSV(1,:),'k--',Hr,RMSINTV(1,:),'k-');
hold on;
for j=2:4,
    semilogy(Hr,RMSINSV(j,:),'k--',Hr,RMSINTV(j,:),'k-');
end;
ylabel('RMS Vertical Err. [m]');
xlabel('Time [hr]');
%
figure;
semilogy(Hr,RMSGPSH,'k:',Hr,RMSINSH(1,:),'k--',Hr,RMSINTH(1,:),'k-');
legend('GPS','INS','GPS/INS');
title('Loosely Coupled GPS/INS Integration Performances');
hold on;
for j=2:4,
    semilogy(Hr,RMSINSH(j,:),'k--',Hr,RMSINTH(j,:),'k-');
end;
ylabel('RMS Horizontal Error [meter]');
xlabel('Time [hr]');
figure;
semilogy(Hr,RMSGPSV,'k:',Hr,RMSINSV(1,:),'k--',Hr,RMSINTV(1,:),'k-');
legend('GPS','INS','GPS/INS');
title('Loosely Coupled GPS/INS Integration Performances');
hold on;
for j=2:4,
    semilogy(Hr,RMSINSV(j,:),'k--',Hr,RMSINTV(j,:),'k-');
end;
ylabel('RMS Vertical Error [meter]');
xlabel('Time [hr]');
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  

    
