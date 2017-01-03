%
% Compute PSDs of GPS receiver vehicle tracking errors using TYPE2,
% DAMP2 and DAMP3 tracking filters tuned to the vehicle dynamics of the 
% Figure8 track model, but applied to a stationary receiver.
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
%
% Measurement noise covariance
%
RPR     = sigmaMN^2*eye(NoSats);
%
% Initial simulation conditions
%
Lat     = 40*pi/180;
Lon     = 0;
Alt     = 100;
%
% Track parameters
%
TrackLength     = 1500; % meter
Speed           = 25;   % meter
CrossOverHeight = 10;   % meter
%
% Vehicle dynamic model parameters
%
dt      = 1;         % discrete time step
RMSPosN = 212.9304;  % meter
RMSPosE = 70.9768;   % meter
RMSPosD = 3.5361;    % meter
RMSVelN = 22.3017;   % m/s
RMSVelE = 14.8678;   % m/s
RMSVelD = 0.37024;   % m/s
RMSAccN = 2.335;     % m/s/s
RMSAccE = 3.1134;    % m/s/s
RMSAccD = 0.038778;  % m/s/s
TauPosN = 13.4097;   % sec
TauPosE = 7.6696;    % sec
TauPosD = 9.6786;    % sec
TauVelN = 9.6786;    % sec
TauVelE = 21.4921;   % sec
TauVelD = 13.4097;   % sec
TauAccN = 13.4097;   % sec
TauAccE = 7.6696;    % sec
TauAccD = 9.6786;    % sec
MSdVN   = 0.02335^2*(dt/.01);    % mean squared delta velocity north
MSdVE   = 0.031134^2*(dt/.01);   % mean squared delta velocity east
MSdVD   = 0.00038771^2*(dt/.01); % mean squared delta velocity down
%
% TYPE2 vehicle tracking model parameters
%
Phi1N   = [1 dt; 0 1];
Phi1E   = [1 dt; 0 1];
Phi1D   = [1 dt; 0 1];
Phi1    = [Phi1N,zeros(2,NoSats+4);zeros(2),Phi1E,zeros(2,NoSats+2);
     zeros(2,4),Phi1D,zeros(2,NoSats);zeros(NoSats,6),PhiPR];
Q1N     = [0,0;0,MSdVN];
Q1E     = [0,0;0,MSdVE];
Q1D     = [0,0;0,MSdVD];
Q1      = [Q1N,zeros(2,NoSats+4);
    zeros(2),Q1E,zeros(2,NoSats+2);
    zeros(2,4),Q1D,zeros(2,NoSats);
    zeros(NoSats,6),QPR];
P1N     = [RMSPosN^2,0;0,RMSVelN^2];
P1E     = [RMSPosE^2,0;0,RMSVelE^2];
P1D     = [RMSPosD^2,0;0,RMSVelD^2];
P1      = [P1N,zeros(2,NoSats+4);
    zeros(2),P1E,zeros(2,NoSats+2);
    zeros(2,4),P1D,zeros(2,NoSats);
    zeros(NoSats,6),P0PR];
%
% Initialize satellite signal delay error estimates at true values
%
x1      = [zeros(6,1);xPR];
x2      = [zeros(6,1);xPR];
x3      = [zeros(9,1);xPR];
%
% DAMP2 vehicle tracking model parameters
%
[Phi2N,Q2N,P2N] = DAMP2Params(RMSAccN,RMSVelN,TauAccN,dt);
[Phi2E,Q2E,P2E] = DAMP2Params(RMSAccE,RMSVelE,TauAccE,dt);
[Phi2D,Q2D,P2D] = DAMP2Params(RMSAccD,RMSVelD,TauAccD,dt);
Phi2    = [Phi2N,zeros(2,NoSats+4);zeros(2),Phi2E,zeros(2,NoSats+2);
     zeros(2,4),Phi2D,zeros(2,NoSats);zeros(NoSats,6),PhiPR];
Q2      = [Q2N,zeros(2,NoSats+4);
    zeros(2),Q2E,zeros(2,NoSats+2);
    zeros(2,4),Q2D,zeros(2,NoSats);
    zeros(NoSats,6),QPR];
P2      = [P2N,zeros(2,NoSats+4);
    zeros(2),P2E,zeros(2,NoSats+2);
    zeros(2,4),P2D,zeros(2,NoSats);
    zeros(NoSats,6),P0PR];
%
% DAMP3 vehicle tracking model parameters
%
[Phi3N,Q3N,P3N] = DAMP3Params(RMSAccN,RMSVelN,RMSPosN,TauAccN,dt);
[Phi3E,Q3E,P3E] = DAMP3Params(RMSAccE,RMSVelE,RMSPosE,TauAccE,dt);
[Phi3D,Q3D,P3D] = DAMP3Params(RMSAccD,RMSVelD,RMSPosD,TauAccD,dt);
Phi3    = [Phi3N,zeros(3,NoSats+6);zeros(3),Phi3E,zeros(3,NoSats+3);
     zeros(3,6),Phi3D,zeros(3,NoSats);zeros(NoSats,9),PhiPR];
Q3      = [Q3N,zeros(3,NoSats+6);
    zeros(3),Q3E,zeros(3,NoSats+3);
    zeros(3,6),Q3D,zeros(3,NoSats);
    zeros(NoSats,9),QPR];
P3      = [P3N,zeros(3,NoSats+6);
    zeros(3),P3E,zeros(3,NoSats+3);
    zeros(3,6),P3D,zeros(3,NoSats);
    zeros(NoSats,9),P0PR];
%
% Simulated vehicle on figure-8 track and 9--11 satellites in view
%
k         = 0;
Hours     = 12;  % Hours of simulated operation
StartTime = 100; % Allow 100 sec settling before sampling data
StopTime  = 7300;
x1hat     = [];
x2hat     = [];
x3hat     = [];
sd1       = [];
xtrue     = [];
XPR       = [];
for t=0:dt:Hours*3600+StartTime,
    %
    % Simulated stationary vehicle state (Pos, Vel, Acc)
    %
    PosN       = 0;
    PosE       = 0;
    PosD       = 0;
    VelN       = 0;
    VelE       = 0;
    VelD       = 0;
    AccN       = 0;
    AccE       = 0;
    AccD       = 0;
    %
    % Initialize all estimates with pseudorandom errors
    %
    if t==0
        x1(1)  = PosN + randn*sqrt(P1(1,1));
        x1(2)  = VelN + randn*sqrt(P1(2,2));
        x1(3)  = PosE + randn*sqrt(P1(3,3));
        x1(4)  = VelE + randn*sqrt(P1(4,4));
        x1(5)  = PosD + randn*sqrt(P1(5,5));
        x1(6)  = VelD + randn*sqrt(P1(6,6));
        x2(1)  = PosN + randn*sqrt(P2(1,1));
        x2(2)  = VelN + randn*sqrt(P2(2,2));
        x2(3)  = PosE + randn*sqrt(P2(3,3));
        x2(4)  = VelE + randn*sqrt(P2(4,4));
        x2(5)  = PosD + randn*sqrt(P2(5,5));
        x2(6)  = VelD + randn*sqrt(P2(6,6));
        x3(1)  = PosN + randn*sqrt(P3(1,1));
        x3(2)  = VelN + randn*sqrt(P3(2,2));
        x3(3)  = AccN + randn*sqrt(P3(3,3));
        x3(4)  = PosE + randn*sqrt(P3(4,4));
        x3(5)  = VelE + randn*sqrt(P3(5,5));
        x3(6)  = AccE + randn*sqrt(P3(6,6));
        x3(7)  = PosD + randn*sqrt(P3(7,7));
        x3(8)  = VelD + randn*sqrt(P3(8,8));
        x3(9)  = AccD + randn*sqrt(P3(9,9));
    end;
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
    % Measured pseudorange variation due to antenna location and unknown
    % satellite signal delays (same for all filters).
    %
    z   = HPR*(HGPS*[PosN;PosE;PosD] + xPR + sigmaMN*randn(NoSats,1));
    %
    % Measurement sensitivity matrices for TYPE2, DAMP2 and DAMP3 filters
    %
    H1 = [HGPS(:,1),zeros(NoSats,1),HGPS(:,2),zeros(NoSats,1),HGPS(:,3),zeros(NoSats,1),HPR];
    H2 = [HGPS(:,1),zeros(NoSats,1),HGPS(:,2),zeros(NoSats,1),HGPS(:,3),zeros(NoSats,1),HPR];
    H3 = [HGPS(:,1),zeros(NoSats,2),HGPS(:,2),zeros(NoSats,2),HGPS(:,3),zeros(NoSats,2),HPR];
    %
    % TYPE2 tracker filter: observational update
    %
    HP1 = H1*P1;
    K1  = HP1'/(HP1*H1' + RPR);
    x1  = x1 + K1*(z - H1*x1);
    P1  = P1 - K1*HP1;
    P1  = (P1+P1')/2;
    %
    % DAMP2 tracker filter: observational update
    %
    HP2 = H2*P2;
    K2  = HP2'/(HP2*H2' + RPR);
    x2  = x2 + K2*(z - H2*x2);
    P2  = P2 - K2*HP2;
    P2  = (P2+P2')/2;
    %
    % DAMP3 tracker filter: observational update
    %
    HP3 = H3*P3;
    K3  = HP3'/(HP3*H3' + RPR);
    x3  = x3 + K3*(z - H3*x3);
    P3  = P3 - K3*HP3;
    P3  = (P3+P3')/2;
    if t >= StartTime % exclude the first 100 seconds to allow settling
        k = k + 1;
        xtrue    = [xtrue,[PosN;VelN;PosE;VelE;PosD;VelD]];
        x1hat    = [x1hat,[x1(1);x1(2);x1(3);x1(4);x1(5);x1(6)]];
        x2hat    = [x2hat,[x2(1);x2(2);x2(3);x2(4);x2(5);x2(6)]];
        x3hat    = [x3hat,[x3(1);x3(2);x3(4);x3(5);x3(7);x3(8)]];
    end;
    %
    % TYPE2 filter: temporal update
    %
    x1 = Phi1*x1;
    P1 = Phi1*P1*Phi1' + Q1;
    P1 = (P1+P1')/2;
    %
    % DAMP2 filter: temporal update
    %
    x2 = Phi2*x2;
    P2 = Phi2*P2*Phi2' + Q2;
    P2 = (P2+P2')/2;
    % DAMP3 filter: temporal update
    %
    x3 = Phi3*x3;
    P3 = Phi3*P3*Phi3' + Q3;
    P3 = (P3+P3')/2;
    %
    % Simulated pseudorange delay error: temporal update
    %
    xPR  = PhiPR*xPR + sqrtqPR*randn(NoSats,1);
end;
%
% Compute RMS position errors
%
RMS1N = std(x1hat(1,:)-xtrue(1,:));
RMS1E = std(x1hat(3,:)-xtrue(3,:));
RMS1D = std(x1hat(5,:)-xtrue(5,:));
RMS2N = std(x2hat(1,:)-xtrue(1,:));
RMS2E = std(x2hat(3,:)-xtrue(3,:));
RMS2D = std(x2hat(5,:)-xtrue(5,:));
RMS3N = std(x3hat(1,:)-xtrue(1,:));
RMS3E = std(x3hat(3,:)-xtrue(3,:));
RMS3D = std(x3hat(5,:)-xtrue(5,:));
disp(['TYPE2 Tracker: RMS North Position Error = ',num2str(RMS1N),' [m]']);
disp(['TYPE2 Tracker: RMS East  Position Error = ',num2str(RMS1E),' [m]']);
disp(['TYPE2 Tracker: RMS Down  Position Error = ',num2str(RMS1D),' [m]']);
disp(['DAMP2 Tracker: RMS North Position Error = ',num2str(RMS2N),' [m]']);
disp(['DAMP2 Tracker: RMS East  Position Error = ',num2str(RMS2E),' [m]']);
disp(['DAMP2 Tracker: RMS Down  Position Error = ',num2str(RMS2D),' [m]']);
disp(['DAMP3 Tracker: RMS North Position Error = ',num2str(RMS3N),' [m]']);
disp(['DAMP3 Tracker: RMS East  Position Error = ',num2str(RMS3E),' [m]']);
disp(['DAMP3 Tracker: RMS Down  Position Error = ',num2str(RMS3D),' [m]']);
%
% Compute, smooth and plot Power Spectral Densities of Position Errors
%
freq = (0:2048)/4096/dt;
L    = length(xtrue(1,:))-4095;
figure;
Pyy = zeros(1,4096);
N   = 0;
for k=1:10:L,
    Y   = fft((x1hat(1,k:k+4095)-xtrue(1,k:k+4095)),4096);
    Pyy = Pyy + Y.*conj(Y);
    N   = N + 1;
end;
Pyy = Pyy/4096/N;
subplot(3,1,1),
loglog(freq,Pyy(1:2049),'k-');
title('Relative PSDs of Tracker Position Errors (Receiver Stationary)');
ylabel('North');
hold on;
Pyy = zeros(1,4096);
N   = 0;
for k=1:10:L,
    Y   = fft((x1hat(3,k:k+4095)-xtrue(3,k:k+4095)),4096);
    Pyy = Pyy + Y.*conj(Y);
    N   = N + 1;
end;
Pyy = Pyy/4096/N;
subplot(3,1,2),
loglog(freq,Pyy(1:2049),'k-');
ylabel('East');
hold on;
Pyy = zeros(1,4096);
N   = 0;
for k=1:10:L,
    Y   = fft((x1hat(5,k:k+4095)-xtrue(5,k:k+4095)),4096);
    Pyy = Pyy + Y.*conj(Y);
    N   = N + 1;
end;
Pyy = Pyy/4096/N;
subplot(3,1,3),
loglog(freq,Pyy(1:2049),'k-');
ylabel('Down');
xlabel('Frequency [Hz]');
hold on;
%
%figure;
Pyy = zeros(1,4096);
N   = 0;
for k=1:10:L,
    Y   = fft((x2hat(1,k:k+4095)-xtrue(1,k:k+4095)),4096);
    Pyy = Pyy + Y.*conj(Y);
    N   = N + 1;
end;
Pyy = Pyy/4096/N;
subplot(3,1,1),
loglog(freq,Pyy(1:2049),'k--');
%title('Relative PSD of DAMP2 Tracker Position Errors (Receiver Stationary)');
%ylabel('North');
Pyy = zeros(1,4096);
N   = 0;
for k=1:10:L,
    Y   = fft((x2hat(3,k:k+4095)-xtrue(3,k:k+4095)),4096);
    Pyy = Pyy + Y.*conj(Y);
    N   = N + 1;
end;
Pyy = Pyy/4096/N;
subplot(3,1,2),
loglog(freq,Pyy(1:2049),'k--');
%ylabel('East');
Pyy = zeros(1,4096);
N   = 0;
for k=1:10:L,
    Y   = fft((x2hat(5,k:k+4095)-xtrue(5,k:k+4095)),4096);
    Pyy = Pyy + Y.*conj(Y);
    N   = N + 1;
end;
Pyy = Pyy/4096/N;
subplot(3,1,3),
loglog(freq,Pyy(1:2049),'k--');
%ylabel('Down');
%xlabel('Frequency [Hz]');
%
%figure;
Pyy = zeros(1,4096);
N   = 0;
for k=1:10:L,
    Y   = fft((x3hat(1,k:k+4095)-xtrue(1,k:k+4095)),4096);
    Pyy = Pyy + Y.*conj(Y);
    N   = N + 1;
end;
Pyy = Pyy/4096/N;
subplot(3,1,1),
loglog(freq,Pyy(1:2049),'k:');
%title('Relative PSD of DAMP3 Tracker Position Errors (Receiver Stationary)');
%ylabel('North');
hold off;
legend('TYPE2','DAMP2','DAMP3');
Pyy = zeros(1,4096);
N   = 0;
for k=1:10:L,
    Y   = fft((x3hat(3,k:k+4095)-xtrue(3,k:k+4095)),4096);
    Pyy = Pyy + Y.*conj(Y);
    N   = N + 1;
end;
Pyy = Pyy/4096/N;
subplot(3,1,2),
loglog(freq,Pyy(1:2049),'k:');
hold off;
%ylabel('East');
Pyy = zeros(1,4096);
N   = 0;
for k=1:10:L,
    Y   = fft((x3hat(5,k:k+4095)-xtrue(5,k:k+4095)),4096);
    Pyy = Pyy + Y.*conj(Y);
    N   = N + 1;
end;
Pyy = Pyy/4096/N;
subplot(3,1,3),
loglog(freq,Pyy(1:2049),'k:');
hold off;
%ylabel('Down');
%xlabel('Frequency [Hz]');
%
% Calculate and plot autocorrelation functions
%
NSamp = 43201;
x1tilde = x1hat-xtrue;
x2tilde = x2hat-xtrue;
x3tilde = x3hat-xtrue;
k = 0;
AutoCorr = zeros(9,200+1);
for lag=0:200,
    k          = k + 1;
    LagTime(k) = lag*dt;
    N          = 0;
    for j=1:NSamp-lag;
        N             = N + 1;
        AutoCorr(1,k) = AutoCorr(1,k) + x1tilde(1,j)*x1tilde(1,j+lag);
        AutoCorr(2,k) = AutoCorr(2,k) + x1tilde(3,j)*x1tilde(3,j+lag);
        AutoCorr(3,k) = AutoCorr(3,k) + x1tilde(5,j)*x1tilde(5,j+lag);
        AutoCorr(4,k) = AutoCorr(4,k) + x2tilde(1,j)*x2tilde(1,j+lag);
        AutoCorr(5,k) = AutoCorr(5,k) + x2tilde(3,j)*x2tilde(3,j+lag);
        AutoCorr(6,k) = AutoCorr(6,k) + x2tilde(5,j)*x2tilde(5,j+lag);
        AutoCorr(7,k) = AutoCorr(7,k) + x3tilde(1,j)*x3tilde(1,j+lag);
        AutoCorr(8,k) = AutoCorr(8,k) + x3tilde(3,j)*x3tilde(3,j+lag);
        AutoCorr(9,k) = AutoCorr(9,k) + x3tilde(5,j)*x3tilde(5,j+lag);
    end;
    Threshold(k) = 1/exp(1);
    for j=1:9,
        AutoCorr(j,k) = AutoCorr(j,k)/N;
    end;
end;
NLags = k;
%
for j=1:9,
    Normalizer = AutoCorr(j,1);
    for k=1:NLags,
        AutoCorr(j,k) = AutoCorr(j,k)/Normalizer;
    end;
end;
TH      = 1/exp(1);
TYPE2TauPosN = interp1(AutoCorr(1,:),LagTime,TH);
TYPE2TauPosE = interp1(AutoCorr(2,:),LagTime,TH);
TYPE2TauPosD = interp1(AutoCorr(3,:),LagTime,TH);
DAMP2TauPosN = interp1(AutoCorr(4,:),LagTime,TH);
DAMP2TauPosE = interp1(AutoCorr(5,:),LagTime,TH);
DAMP2TauPosD = interp1(AutoCorr(6,:),LagTime,TH);
DAMP3TauPosN = interp1(AutoCorr(7,:),LagTime,TH);
DAMP3TauPosE = interp1(AutoCorr(8,:),LagTime,TH);
DAMP3TauPosD = interp1(AutoCorr(9,:),LagTime,TH);
disp(['TYPE2 N. Position Correlation Time = ',num2str(TYPE2TauPosN),' sec']);
disp(['TYPE2 E. Position Correlation Time = ',num2str(TYPE2TauPosE),' sec']);
disp(['TYPE2 Vertical Position Corr. Time = ',num2str(TYPE2TauPosD),' sec']);
disp(['DAMP2 N. Velocity Correlation Time = ',num2str(DAMP2TauPosN),' sec']);
disp(['DAMP2 E. Velocity Correlation Time = ',num2str(DAMP2TauPosE),' sec']);
disp(['DAMP2 Vertical Velocity Corr. Time = ',num2str(DAMP2TauPosD),' sec']);
disp(['DAMP3 N. Acceler. Correlation Time = ',num2str(DAMP3TauPosN),' sec']);
disp(['DAMP3 E. Acceler. Correlation Time = ',num2str(DAMP3TauPosE),' sec']);
disp(['DAMP3 Vertical Acceler. Corr. Time = ',num2str(DAMP3TauPosD),' sec']);
figure;
subplot(3,1,1),
plot(LagTime,AutoCorr(1,:),'k-',LagTime,AutoCorr(4,:),'k--',LagTime,AutoCorr(7,:),'k:',LagTime,Threshold,'k-');
legend('TYPE2','DAMP2','DAMP3','1/e');
title('Position Error Autocorrelation Functions (Receiver Stationery)');
%xlabel('Lag Time [sec]');
ylabel('North Pos.');
%
subplot(3,1,2),
plot(LagTime,AutoCorr(2,:),'k-',LagTime,AutoCorr(5,:),'k--',LagTime,AutoCorr(8,:),'k:',LagTime,Threshold,'k-');
%legend('N','E','D','1/e');
%title('DAMP2 Position Error Autocorrelation Functions (Receiver Stationery)');
%xlabel('Lag Time [sec]');
ylabel('East Pos.');
%
subplot(3,1,3),
plot(LagTime,AutoCorr(3,:),'k-',LagTime,AutoCorr(6,:),'k--',LagTime,AutoCorr(9,:),'k:',LagTime,Threshold,'k-');
%legend('N','E','D','1/e');
%title('DAMP3 Position Error  Autocorrelation Functions  (Receiver Stationery)');
xlabel('Lag Time [sec]');
ylabel('Altitude');
