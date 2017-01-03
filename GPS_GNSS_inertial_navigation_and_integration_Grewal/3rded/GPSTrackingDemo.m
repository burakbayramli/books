%
% Comparison of TYPE2, DAMP2 and DAMP3 vehicle tracking models using
% a simulator for 1.5 km figure-8 track at 90 kph average speed
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
Speed           = 25;   % meter/sec (90 kph)
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
x4      = [zeros(2,1);xPR];
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
% 1D Figure-8 tracker model
%
Omega0      = 2*pi*Speed/TrackLength; % Mean angular speed [rad/sec]
TauPhiDot   = 10; % Correlation time-constant for speed changes [sec]
SigmaPhiDot = Omega0/10; % 10 percent RMS speed variation
SigmaPhi    = 10/TrackLength; % RMS track phase uncertainty equiv 10 m
PhiState    = [1,0;dt,exp(-dt/TauPhiDot)];
Phi4        = [PhiState,zeros(2,NoSats);
               zeros(NoSats,2),PhiPR];
q4          = SigmaPhiDot^2*(1-exp(-2*dt/TauPhiDot));
Q4          = [zeros(1,NoSats+2);0,q4,zeros(1,NoSats);
               zeros(NoSats,2),QPR];
P4          = [SigmaPhi^2,zeros(1,NoSats+1);0,SigmaPhiDot^2,zeros(1,NoSats);
               zeros(NoSats,2),P0PR];
%
% Simulated vehicle on figure-8 track and 9--11 satellites in view
%
k         = 0;
hours     = 2;   % hours of simulated time
StartTime = 100; % Allow 100 sec settling before sampling data
x1hat     = [];
x2hat     = [];
x3hat     = [];
x4hat     = [];
sd1       = [];
xtrue     = [];
XPR       = [];
State     = [0;0]; % Track state variables are track phase and phase rate
Phi       = [];
PhiHat    = [];
PhiDot    = [];
PhiDotHat = [];
for t=0:dt:hours*3600*dt+StartTime,
    %
    % Simulated vehicle state (Pos, Vel, Acc)
    %
    phi       = State(1);
    phidot    = State(2);
    [VehState,dPosdphi] = Fig8Mod1D(t,phi,TrackLength,Speed,CrossOverHeight);
    PosN       = VehState(1);
    PosE       = VehState(2);
    PosD       = VehState(3);
    VelN       = VehState(4);
    VelE       = VehState(5);
    VelD       = VehState(6);
    AccN       = VehState(7);
    AccE       = VehState(8);
    AccD       = VehState(9);
    %
    % Initialize all estimates with true values
    %
    if t==0
        x1(1)  = PosN;
        x1(2)  = VelN;
        x1(3)  = PosE;
        x1(4)  = VelE;
        x1(5)  = PosD;
        x1(6)  = VelD;
        x2(1)  = PosN;
        x2(2)  = VelN;
        x2(3)  = PosE;
        x2(4)  = VelE;
        x2(5)  = PosD;
        x2(6)  = VelD;
        x3(1)  = PosN;
        x3(2)  = VelN;
        x3(3)  = AccN;
        x3(4)  = PosE;
        x3(5)  = VelE;
        x3(6)  = AccE;
        x3(7)  = PosD;
        x3(8)  = VelD;
        x3(9)  = AccD;
    end;
    %
    % Core GPS pseudorange measurement sensitivities for all satellites,
    % visible or not.
    %
    HGPS = HSatSim(t,Lat,Lon,Alt);
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
    H1      = [HGPS(:,1),zeros(NoSats,1),HGPS(:,2),zeros(NoSats,1),HGPS(:,3),zeros(NoSats,1),HPR];
    H2      = [HGPS(:,1),zeros(NoSats,1),HGPS(:,2),zeros(NoSats,1),HGPS(:,3),zeros(NoSats,1),HPR];
    H3      = [HGPS(:,1),zeros(NoSats,2),HGPS(:,2),zeros(NoSats,2),HGPS(:,3),zeros(NoSats,2),HPR];
    phihat  = x4(1); % Use extended Kalman filter for Fig8 tracker
    [VehState4,dPosdphi] = Fig8Mod1D(t,phihat,TrackLength,Speed,CrossOverHeight);
    H4 = [HGPS*dPosdphi,zeros(NoSats,1),HPR];
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
    %
    % 1D Figure-8 tracker filter: observational update
    %
    HP4 = H4*P4;
    K4  = HP4'/(HP4*H4' + RPR);
    x4  = x4 + K4*(z - H4*x4);
    P4  = P4 - K4*HP4;
    P2  = (P2+P2')/2;
    if t >= StartTime % exclude the first 100 seconds to allow settling
        k = k + 1;
        Time(k)  = t - StartTime;
        xtrue    = [xtrue,[PosN;VelN;PosE;VelE;PosD;VelD]];
        sd1      = [sd1,[sqrt(P1(1,1));sqrt(P1(2,2));sqrt(P1(3,3));sqrt(P1(4,4));sqrt(P1(5,5));sqrt(P1(6,6))]];
        x1hat    = [x1hat,[x1(1);x1(2);x1(3);x1(4);x1(5);x1(6)]];
        x2hat    = [x2hat,[x2(1);x2(2);x2(3);x2(4);x2(5);x2(6)]];
        x3hat    = [x3hat,[x3(1);x3(2);x3(4);x3(5);x3(7);x3(8)]];
        phihat   = x4(1);
        phidothat = x4(2);
        Phi      = [Phi,phi];
        PhiHat   = [PhiHat,phihat];
        PhiDot   = [PhiDot,phidot];
        PhiDotHat= [PhiDotHat,phidothat];
        [VehState4,dPosdphi] = Fig8Mod1D(t,phihat,TrackLength,Speed,CrossOverHeight);
        x4hat    = [x4hat,[VehState4(1);VehState4(4);VehState4(2);VehState4(5);VehState4(3);VehState4(6)]];
        for j=1:NoSats,
            sdPR(j) = sqrt(P1(j+6,j+6));
        end;
        %
        % Satellite pseudorange error estimation data, including
        %   true psorange error [m]
        %   estimated pseudorange error [m]
        %   standard deviation of estimation uncertainty [m]
        %
        % They are plotted to show that:
		%	1. Satellites not in view have RMS uncertainties of around 10 m
        %      estimated delays of zero m, and RMS tru values around 10 m.
        %   2. Satellites in view have RMS uncertainties of around 6 m,
        %      and estimated delays tracking true values within 6 m RMS.
        %
        XPR           = [XPR,[xPR;x1(7:35);sdPR';x2(7:35);x3(10:38)]];
		SatsInView(k) = NoSatsAvail;
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
    % 1D Figure-8 tracker filter: temporal update
    %
    x4    = Phi4*x4;
    P4 = Phi4*P4*Phi4' + Q4;
    P4 = (P4+P4')/2;
    %
    % Along-track vehicle phase update
    %
    State = PhiState*State + randn(1)*[0;1];
    phi   = State(1);
    %
    % Simulated pseudorange delay error: temporal update
    %
    xPR  = PhiPR*xPR + sqrtqPR*randn(NoSats,1);
end;
%
% Display TYPE2 tracker performance statistics
%
T2RMSPosErrN = std(x1hat(1,:)-xtrue(1,:));
T2RMSPosErrE = std(x1hat(3,:)-xtrue(3,:));
T2RMSPosErrD = std(x1hat(5,:)-xtrue(5,:));
T2RMSVelErrN = std(x1hat(2,:)-xtrue(2,:));
T2RMSVelErrE = std(x1hat(4,:)-xtrue(4,:));
T2RMSVelErrD = std(x1hat(6,:)-xtrue(6,:));
disp('TYPE2 Tracker:');
disp(['   RMS N Pos Err = ',num2str(T2RMSPosErrN),' [m]']);
disp(['   RMS E Pos Err = ',num2str(T2RMSPosErrE),' [m]']);
disp(['   RMS D Pos Err = ',num2str(T2RMSPosErrD),' [m]']);
disp(['   RMS N Vel Err = ',num2str(T2RMSVelErrN),' [m]']);
disp(['   RMS E Vel Err = ',num2str(T2RMSVelErrE),' [m]']);
disp(['   RMS D Vel Err = ',num2str(T2RMSVelErrD),' [m]']);
%
% Display DAMP2 tracker performance statistics
%
D2RMSPosErrN = std(x2hat(1,:)-xtrue(1,:));
D2RMSPosErrE = std(x2hat(3,:)-xtrue(3,:));
D2RMSPosErrD = std(x2hat(5,:)-xtrue(5,:));
D2RMSVelErrN = std(x2hat(2,:)-xtrue(2,:));
D2RMSVelErrE = std(x2hat(4,:)-xtrue(4,:));
D2RMSVelErrD = std(x2hat(6,:)-xtrue(6,:));
disp('DAMP2 Tracker:');
disp(['   RMS N Pos Err = ',num2str(D2RMSPosErrN),' [m]']);
disp(['   RMS E Pos Err = ',num2str(D2RMSPosErrE),' [m]']);
disp(['   RMS D Pos Err = ',num2str(D2RMSPosErrD),' [m]']);
disp(['   RMS N Vel Err = ',num2str(D2RMSVelErrN),' [m]']);
disp(['   RMS E Vel Err = ',num2str(D2RMSVelErrE),' [m]']);
disp(['   RMS D Vel Err = ',num2str(D2RMSVelErrD),' [m]']);
%
% Display DAMP3 tracker performance statistics
%
D3RMSPosErrN = std(x3hat(1,:)-xtrue(1,:));
D3RMSPosErrE = std(x3hat(3,:)-xtrue(3,:));
D3RMSPosErrD = std(x3hat(5,:)-xtrue(5,:));
D3RMSVelErrN = std(x3hat(2,:)-xtrue(2,:));
D3RMSVelErrE = std(x3hat(4,:)-xtrue(4,:));
D3RMSVelErrD = std(x3hat(6,:)-xtrue(6,:));
disp('DAMP3 Tracker:');
disp(['   RMS N Pos Err = ',num2str(D3RMSPosErrN),' [m]']);
disp(['   RMS E Pos Err = ',num2str(D3RMSPosErrE),' [m]']);
disp(['   RMS D Pos Err = ',num2str(D3RMSPosErrD),' [m]']);
disp(['   RMS N Vel Err = ',num2str(D3RMSVelErrN),' [m]']);
disp(['   RMS E Vel Err = ',num2str(D3RMSVelErrE),' [m]']);
disp(['   RMS D Vel Err = ',num2str(D3RMSVelErrD),' [m]']);
%
% Display FIG8 tracker performance statistics
%
T4RMSPosErrN = std(x4hat(1,:)-xtrue(1,:));
T4RMSPosErrE = std(x4hat(3,:)-xtrue(3,:));
T4RMSPosErrD = std(x4hat(5,:)-xtrue(5,:));
T4RMSVelErrN = std(x4hat(2,:)-xtrue(2,:));
T4RMSVelErrE = std(x4hat(4,:)-xtrue(4,:));
T4RMSVelErrD = std(x4hat(6,:)-xtrue(6,:));
disp('Fig8Mod1D Tracker:');
disp(['   RMS N Pos Err = ',num2str(T4RMSPosErrN),' [m]']);
disp(['   RMS E Pos Err = ',num2str(T4RMSPosErrE),' [m]']);
disp(['   RMS D Pos Err = ',num2str(T4RMSPosErrD),' [m]']);
disp(['   RMS N Vel Err = ',num2str(T4RMSVelErrN),' [m]']);
disp(['   RMS E Vel Err = ',num2str(T4RMSVelErrE),' [m]']);
disp(['   RMS D Vel Err = ',num2str(T4RMSVelErrD),' [m]']);
%
% Plot 2D plan view of true trajectory and estimated trajectories
%
figure;
plot(x1hat(3,:),x1hat(1,:),'g-',x2hat(3,:),x2hat(1,:),'b-',x3hat(4,:),x3hat(1,:),'y-',x4hat(3,:),x4hat(1,:),'r-',xtrue(3,:),xtrue(1,:),'k-');axis equal;
title('Plan View of Estimated Locations and True Locations');
legend('TYPE2','DAMP2','DAMP3','FIG8','TRUE');
%
% Plot TYPE2 tracker navigation errors
%
figure;
subplot(3,1,1),plot(Time/3600,sd1(1,:),'k--',Time/3600,-sd1(1,:),'k--',Time/3600,x1hat(1,:)-xtrue(1,:),'k-');
title('TYPE2 Simulation: Position Error');
ylabel('North');
subplot(3,1,2),plot(Time/3600,sd1(3,:),'k--',Time/3600,-sd1(3,:),'k--',Time/3600,x1hat(3,:)-xtrue(3,:),'k-');
ylabel('East');
subplot(3,1,3);plot(Time/3600,sd1(5,:),'k--',Time/3600,-sd1(5,:),'k--',Time/3600,x1hat(5,:)-xtrue(5,:),'k-');
ylabel('Down');
xlabel('Time [hr]');
%
figure;
subplot(3,1,1),plot(Time/3600,sd1(2,:),'k--',Time/3600,-sd1(2,:),'k--',Time/3600,x1hat(2,:)-xtrue(2,:),'k-');
title('TYPE2 Simulation: Velocity Error');
ylabel('North');
subplot(3,1,2),plot(Time/3600,sd1(4,:),'k--',Time/3600,-sd1(4,:),'k--',Time/3600,x1hat(4,:)-xtrue(4,:),'k-');
ylabel('East');
subplot(3,1,3),plot(Time/3600,sd1(6,:),'k--',Time/3600,-sd1(6,:),'k--',Time/3600,x1hat(6,:)-xtrue(6,:),'k-');
ylabel('Down');
xlabel('Time [hr]');
%
% For each satellite (whether in view or not), plot calculated +/1 RMS
% signal delay estimation uncertainty, true delay and estimated delay.
%
for j=0:6
    figure;
    subplot(4,1,1),plot(Time/3600,XPR(4*j+1,:),'b-',Time/3600,XPR(4*j+1+NoSats,:),'k-',Time/3600,XPR(4*j+1+2*NoSats,:),'k--',Time/3600,-XPR(4*j+1+2*NoSats,:),'k--');
    ylabel(['SatNo. ',num2str(4*j+1)]);
    title('TYPE2 Tracker: User Equivalent Range Errors [m] from Propagation Delays');
    subplot(4,1,2),plot(Time/3600,XPR(4*j+2,:),'b-',Time/3600,XPR(4*j+2+NoSats,:),'k-',Time/3600,XPR(4*j+2+2*NoSats,:),'k--',Time/3600,-XPR(4*j+2+2*NoSats,:),'k--');
    ylabel(['SatNo. ',num2str(4*j+2)]);
    subplot(4,1,3),plot(Time/3600,XPR(4*j+3,:),'b-',Time/3600,XPR(4*j+3+NoSats,:),'k-',Time/3600,XPR(4*j+3+2*NoSats,:),'k--',Time/3600,-XPR(4*j+3+2*NoSats,:),'k--');
    ylabel(['SatNo. ',num2str(4*j+3)]);
    subplot(4,1,4),plot(Time/3600,XPR(4*j+4,:),'b-',Time/3600,XPR(4*j+4+NoSats,:),'k-',Time/3600,XPR(4*j+4+2*NoSats,:),'k--',Time/3600,-XPR(4*j+4+2*NoSats,:),'k--');
    ylabel(['SatNo. ',num2str(4*j+4)]);
    xlabel('Time [hr]');
end;
figure;
subplot(4,1,1),plot(Time/3600,XPR(NoSats,:),'b-',Time/3600,XPR(NoSats+NoSats,:),'k-',Time/3600,XPR(NoSats+2*NoSats,:),'k--',Time/3600,-XPR(NoSats+2*NoSats,:),'k--');
ylabel(['SatNo. ',num2str(NoSats)]);
title('TYPE2 Tracker: User Equivalent Range Errors [m] from Propagation Delays');
subplot(4,1,2),plot(Time/3600,SatsInView,'k-');
ylabel('# in View');
subplot(4,1,3),plot(Time/3600,Phi,'b-',Time/3600,PhiHat,'r-');
legend('true','est.');
ylabel('Track Phase [rad]');
subplot(4,1,4),plot(Time/3600,PhiDot,'b-',Time/3600,PhiDotHat,'r-');
legend('true','est.');
ylabel('Phase Rate [rad/s]');
xlabel('Time [hr]');
%
% Plot Power Spectral Densities of Position Errors
%
freq = (0:2048)/4096/dt;
figure;
subplot(3,1,1),
Y   = fft((x1hat(1,:)-xtrue(1,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
title('Relative PSD of TYPE2 Tracker Position Errors');
ylabel('North');
subplot(3,1,2),
Y   = fft((x1hat(3,:)-xtrue(3,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
ylabel('East');
subplot(3,1,3),
Y   = fft((x1hat(5,:)-xtrue(5,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
ylabel('Down');
xlabel('Frequency [Hz]');
%
figure;
subplot(3,1,1),
Y   = fft((x2hat(1,:)-xtrue(1,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
title('Relative PSD of DAMP2 Tracker Position Errors');
ylabel('North');
subplot(3,1,2),
Y   = fft((x2hat(3,:)-xtrue(3,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
ylabel('East');
subplot(3,1,3),
Y   = fft((x2hat(5,:)-xtrue(5,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
ylabel('Down');
xlabel('Frequency [Hz]');
%
figure;
subplot(3,1,1),
Y   = fft((x3hat(1,:)-xtrue(1,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
title('Relative PSD of DAMP3 Tracker Position Errors');
ylabel('North');
subplot(3,1,2),
Y   = fft((x3hat(3,:)-xtrue(3,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
ylabel('East');
subplot(3,1,3),
Y   = fft((x3hat(5,:)-xtrue(5,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
ylabel('Down');
xlabel('Frequency [Hz]');
%
figure;
subplot(3,1,1),
Y   = fft((x4hat(1,:)-xtrue(1,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
title('Relative PSD of FIG8 Tracker Position Errors');
ylabel('North');
subplot(3,1,2),
Y   = fft((x4hat(3,:)-xtrue(3,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
ylabel('East');
subplot(3,1,3),
Y   = fft((x4hat(5,:)-xtrue(5,:)),4096);
Pyy = Y.*conj(Y)/4096;
loglog(freq,Pyy(1:2049),'k-');
ylabel('Down');
xlabel('Frequency [Hz]');
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
