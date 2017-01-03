%
% Simulated INS vertical channel covariances without damping,
% compared to altimeter and GPS damping
%
clear all;
close all;
%
% GPS model
%
global RA PA
YUMAdata;         % 08 March 2006 almanac data with 29 satellites
NoSats  = 29;     % Number Of Satellites
Dt      = 1;      % time interval between GPS pseudoranges
s15     = 0.25881904510252; % sine of 15 degrees (horizon filter)
sigma0  = 1*sqrt(3);   % initial RMS position uncertainty (radial)
sigmap  = 10;     % RMS pseudorange uncertainty (steady-state)
sigmaMN = 10;     % RMS pseudorange measurement noise (white, zero-mean)
taup    = 60;     % correlation time-constant of pseudorange errors
sigmaPR = sigmap*sqrt(1 - exp(-2*Dt/taup))/Dt; % Pseudorange dynamic noise
%
% GPS model parameters
%
RGPS    = sigmaMN^2*eye(NoSats); % GPS measurement noise covariance
%
% Initial covariance of uncertainty
%
Pnn     = sigmap^2*eye(NoSats);
%
% State transition matrix (constant)
%
ep      = exp(-Dt/taup); % pseudorange
Phin    = [ep*eye(NoSats)];
%
% Covariance of dynamic disturbance noise
%
Qee     = [zeros(6,9);zeros(3,6),sigmajerk^2*Dt^2/3*eye(3)];
Qnn     = sigmaPR^2*Dt^2*eye(NoSats);
Q       = [Qee,zeros(9,NoSats);zeros(NoSats,9),Qnn];

%
% Initial simulation conditions
%
Lat     = 40*pi/180;
Lon     = 0;
Alt     = 100;
%
% Time between measurements
%
DeltaT  = 1; % [sec]
%
% Altimeter model
%
Raltimeter     = 10;      % RMS altimeter noise [meter]
SigmaAltitude  = 100;     % RMS altimeter altitude offset error [meter]
TauAltimeter   = 10*3600; % altimeter offset correlation time [10 hours]
Qaltimeter     = (1-exp(-2*DeltaT/TauAltimeter))*SigmaAltitude^2;
%
% Vertical channel dynamics
%
TSchuler       = 5046;    % Schuler period [sec]
%
% State transition matrices, non-augmented and augmented
%
Phi2           = .5*exp(DeltaT/TSchuler)*[1,TSchuler;1/TSchuler,1] + .5*exp(-DeltaT/TSchuler)*[1,-TSchuler;-1/TSchuler,1];
Phi3           = [Phi2,zeros(2,1);zeros(1,2),exp(-DeltaT/TauAltimeter)];
Phi31          = [Phi2;zeros(2,29);zeros(29,2);Phin];
H3             = [1,0,1];
H2             = [1,0];
Legends        = [];
hours          = 10;   % Simulated hours of operation
mpsprh         = 1;    % meter/sec/root-hour accelerometer noise
Qaccelerometer = mpsprh^2*DeltaT/3600;
Q2             = [0,0;0,Qaccelerometer];
Q3             = [Q2,zeros(2,1);zeros(1,2),Qaltimeter];
P2             = [1,0;0,1e-4]; % 1 m alt. uncert., 10 mm/s vel. uncert.
P3             = [P2,zeros(2,1);zeros(1,2),1];
k              = 1;
Time(k)        = 0;
Sigma2(k)      = sqrt(P2(1,1));
Sigma3(k)      = sqrt(P3(1,1));
Sigma31(k)     = sqrt(P31(1,1));
for t = DeltaT:DeltaT:3600*hours,
    k         = k + 1;
    Time(k)   = t/3600; % time in hours
    P2        = Phi2*P2*Phi2' + Q2;
    Sigma2(k) = sqrt(P2(1,1));
    P3        = Phi3*P3*Phi3' + Q3; % a priori
    K3        = P3*H3'/(H3*P3*H3'+ Raltimeter);
    P3        = P3 - K3*H3*P3; % a posteriori
    P3        = .5*(P3 + P3');
    Sigma3(k) = sqrt(P3(1,1));
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
    % Kalman filter using pseudoranges
    %
    H31         = [HGPS(:,3)];
    HP          = H31*P31;
    P31         = P31 - (HP'/(HP*H31' + R))*HP;
    P31         = (P31+P31')/2;
    P31         = Phi*P31*Phi' + Q31;
    P31         = (P31+P31')/2;
    Sigma31(k)  = sqrt(P31(1,1));
    %
    % Kalman filter using receiver altitude (TBD)
    %
end;
semilogy(Time,Sigma2,'k-',Time,Sigma3,'k:');
text(Time(length(Time))-.01,Sigma2(length(Time)),Legend,'HorizontalAlignment','right','VerticalAlignment','bottom');
hold off;
legend('Und.','Alt.','GPS');
title('INS Vertical Channel Uncertainties: Undamped, Altimeter Damped & GPS Damped');
xlabel('INS Running Time from Initialization [hours]');
ylabel('RMS INS Altitude Uncertainty [meter]');

    

    