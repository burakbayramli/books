%%  
%%  Grewal, Andrews, and Bartone,
%%  Global Navigation Satellite Systems, Inertial Navigation Systems, and Integration
%%  John Wiley & Sons, 2013.
%%  
%
% PERFORMANCE ANALYSIS OF INTEGRATED GNSS/INS NAVIGATION SYSTEM,
% OPERATING ON A 100-KM FIGURE-8 TEST TRACK.
%
% THE SIMULATED INS IS GIMBALED, BUT THE ALTERNATIVE M-FILE
% "Fig8TRackSimRPY.m" CAN BE USED TO SIMULATE ACCELERATIONS
% AND ROTATION RATES SENSED IN BODY-FIXED (RPY) COORDINATES
% FOR STRAPDOWN SIMULATIONS.
%
close all;
clear all;
%
% Earth model
%
REarth        = 6371009;    % mean radius of earth [m]
%
% Test site location
%
Lat       = 39.7931*pi/180;         % 
Lon       = -86.2389*pi/180;        % Approximate location of
Alt       = 221;                    % Indianapolis Motor Speedway
LatRad    = Lat*pi/180;             %
LonRad    = Lon*pi/189;             %
cLat      = cos(LatRad);
%
% 15-degree horizon mask limit for satellite selection
%
s15 = sin(15*pi/180); % sine of mask angle
%
% Set simulation time-step
%
dt          = 10;  % 1-second discrete time-step
%
%%%%   
%%%%         %%%%%%%        %%%%    %%%%%        %%%%%%             %%%%%%
%%%%      %%%%%%%%%%%%      %%%%%   %%%%%      %%%%%%%%%%         %%%%%%%%%%
%%%%    %%%%%     %%%%%%     %%%%%  %%%%     %%%%%    %%%%%     %%%%%   %%%%%
%%%%   %%%%                  %%%%%% %%%%     %%%%%              %%%%%
%%%%   %%%%     %%%%%%%%     %%%% %%%%%%      %%%%%%%%%%%%       %%%%%%%%%%%%
%%%%   %%%%     %%%%%%%%     %%%%  %%%%%        %%%%%%%%%%%        %%%%%%%%%%% 
%%%%    %%%%%      %%%       %%%%   %%%%               %%%%                %%%% 
%%%%      %%%%%%%%%%%%%%    %%%%%    %%%%%    %%%%%   %%%%        %%%%%   %%%%  
%%%%         %%%%%%    %%   %%%%%     %%%%      %%%%%%%%%           %%%%%%%%%   
%%%%
%  
% Set up the GNSS (GPS) simulation database
%
global RA PA ;% globalize satellite ephemerides for use by functions
load('RA');   % loads GPS ephemeris right ascensions for 7 Jul 2012
load('PA');   % loads GPS ephemeris angles from n-to-s equator crossing
%
% Check for data mismatch
%
if length(RA) ~= length(PA), error('Ephemeris data corrupted!'); return;end;
% 
NumSats        = length(RA);  % number of satellites listed in ephemerides
%
% Define the state transition matrix for satellite signal delay
% errors.
%
tauPR = 60; % correlation time [s] of pseudorange errors due to atm. delay
%
PhiPR          = exp(-dt/tauPR)*eye(NumSats);
%
% Satellite pseudorange noise covariance (RPR)
%
sigmaMN        = 10; % 10 m RMS measurement noise
%
RPR            = sigmaMN^2*eye(NumSats);
%
% RMS steady-state exponentially correlated pseudorange error (PR)
%
sigmaPR        = 10; % long-term RMS value [m]
%
% pseudorange error process noise covariance
%
QPR            = sigmaPR^2*(1 - exp(-dt/tauPR))*eye(NumSats);
stdevPR        = sigmaPR*sqrt(1 - exp(-dt/tauPR)); % standard deviation
%
% Initial covariance matrix of pseudorange error uncertainty
%
PPR0           = sigmaPR^2*eye(NumSats); % initial pseudorange uncertainty
%
% Receiver clock model parameters
%
taudrift    = 3600;     % Clock drift rate correlation time [1 hr]
sigmadrift  = 10;       % RMS  drift rate [m/s]
[Fclock,Qclock,Hclock,P0clock] = ClockModel2(sigmadrift,taudrift);
Phiclock       = expm(Fclock*dt); % state transition matrix
%
%%%%    
%%%%    %%%%%%%%    %%%%    %%%%%        %%%%%%    
%%%%    %%%%%%%%    %%%%%   %%%%%      %%%%%%%%%%  
%%%%      %%%%       %%%%%  %%%%     %%%%%%   %%%%%
%%%%      %%%%       %%%%%% %%%%     %%%%%         
%%%%      %%%%       %%%% %%%%%%      %%%%%%%%%%%% 
%%%%      %%%%       %%%%  %%%%%        %%%%%%%%%%%
%%%%      %%%%       %%%%   %%%%               %%%%
%%%%    %%%%%%%%    %%%%%    %%%%%    %%%%%   %%%% 
%%%%    %%%%%%%%    %%%%%     %%%%      %%%%%%%%%  
%%%%    
%
% INS navigation error state vector for 9-state navigation error model
%
NNES           = 9;  % Number of Navigation Error State Variables
%
%  The 9x9 state transition matrix depends on dynamic conditions,
%  so it must be built inside the simulation loop.
%
%
%  The dynamic coupling matrix between sensor compensation errors and
%  INS navigation errors also depends on dynamic conditions.
%
NSCPs          = 9; % Number of Sensor Compensation Parameters
%
% Sensor process noise covariances
%
RMSAccNoise     = 1e-3;     % [m/s/s/sqrt(s)]
RMSGyroNoise    = 1e-5;     % [rad/s/sqrt(s)]
Qnoise          = [zeros(3,9);zeros(3),RMSAccNoise^2*eye(3),zeros(3);zeros(3,6),RMSGyroNoise^2*eye(3)];
%
% Sensor compensation error process noise covariance and initial
% covariance
%
tauSensorComp   = 3600;     % 1-hour corelation time
RMSAccBias      = 9.8e-4;   % 100 micro-g
RMSAccSF        = 1e-4;     % 100 ppm
RMSGyroBias     = 1e-8;    % 10^(-4)*AccBias in G
QSensorComp     = [2*RMSAccBias^2/tauSensorComp*eye(3),zeros(3,6);zeros(3),2*RMSAccSF^2/tauSensorComp*eye(3),zeros(3);zeros(3,6),2*RMSGyroBias^2/tauSensorComp*eye(3)];
FSC9            = FSensorComp(tauSensorComp);
PSensorComp     = [RMSAccBias^2*eye(3),zeros(3,6);zeros(3),RMSAccSF^2*eye(3),zeros(3);zeros(3,6),RMSGyroBias^2*eye(3)];
%
% 18-state dynamic model for INS error propagation: process noise Q
%
Q               = [Qnoise,zeros(9,11+NumSats);zeros(9),QSensorComp,zeros(9,2+NumSats);zeros(2,18),Qclock,zeros(2,NumSats);zeros(NumSats,20),QPR];
%
% Initial covariance of navigation uncertainty (1 m, .01 m/s and 10^3 rad)
%
P0naverr        = [eye(3),zeros(3,6);zeros(3),1e-4*eye(3),zeros(3);zeros(3,6),1e-6*eye(3)];
% 
% Initial covariance of sensor compensation (1e-6 g, 1e-6, 1e-6 rad/s
% 
P0sensorcomp    = [1e-12*eye(3),zeros(3,6);zeros(3),1e-12*eye(3),zeros(3);zeros(3,6),1e-12*eye(3)];

%
% Inital covariance of uncertainty
%
P           = [P0naverr,zeros(9,11+NumSats);zeros(9,9),P0sensorcomp,zeros(9,2+NumSats);zeros(2,18),P0clock,zeros(2,NumSats);zeros(NumSats,20),PPR0];
%
% Initialize plotting variable arrays
%
CEPGNSSINS  = [];
HorGNSSINS  = [];
AltGNSSINS  = [];
VelGNSSINS  = [];
TiltGNSSINS = [];
HeadGNSSINS = [];
RMSAccComp  = [];
RMSGyroComp = [];
RMSclock    = [];
RMSPropDel  = [];
NoSatsUsed  = [];
time        = [];
%
% Set test length
%
hours       = 6;            % Test run time in hours
minutes     = hours*60;
seconds     = minutes*60;
%
COUNT = 0;
%
for t=0:dt:seconds,
    COUNT       = COUNT + 1;
    time        = [time,t];
    [xENU,vENU,aENU,aSensedENU,omegaENU,omegaSensedENU] = Big8TrackSimENU(t,LatRad,LonRad,Alt); 
    HGNSS       = HSatENU(t,LatRad+xENU(2)/REarth,LonRad,Alt);
    NoSatsAvail = NumSats;
    Hclock      = ones(NumSats,1); % clock error sensitivity
    HPR         = eye(NumSats);    % propagation delay sensitivity
    for j=1:NumSats,
        if HGNSS(j,3) > -0.25881904510252 
            NoSatsAvail = NoSatsAvail - 1;
            HGNSS(j,1)  = 0;
            HGNSS(j,2)  = 0;
            HGNSS(j,3)  = 0;
            Hclock(j)   = 0;
            HPR(j,j)    = 0;
        end;
    end;
    H           = [HGNSS,zeros(NumSats,15),Hclock,zeros(NumSats,1),HPR];
    if ~mod(floor(t/3600),2),
        NoSatsUsed  = [NoSatsUsed,NoSatsAvail];
        PHT         = P*H';
        Denom       = H*PHT + RPR;
        K           = PHT/Denom;
        P           = P - K*PHT';
        P           = .5*(P + P');
    else 
        NoSatsUsed  = [NoSatsUsed,0];
        %
        % a posteriori estimated covariances
        %
    end;
    CEPGNSSINS  = [CEPGNSSINS,1.2*sqrt(P(1,1)+P(2,2))/1852];
    HorGNSSINS  = [HorGNSSINS,sqrt(P(1,1)+P(2,2))];
    AltGNSSINS  = [AltGNSSINS,sqrt(P(3,3))];
    VelGNSSINS  = [VelGNSSINS,sqrt(P(4,4)+P(5,5)+P(6,6))];
    TiltGNSSINS = [TiltGNSSINS,sqrt(P(7,7)+P(8,8))];
    HeadGNSSINS = [HeadGNSSINS,sqrt(P(9,9))];
    RMSAccComp  = [RMSAccComp,sqrt([P(10,10);P(11,11);P(12,12);P(13,13);P(14,14);P(15,15)])];
    RMSGyroComp = [RMSGyroComp,sqrt([P(16,16);P(17,17);P(18,18)])];
    RMSclock    = [RMSclock,sqrt([P(19,19);P(20,20)])];
    r           = [];
    for m=21:20+NumSats,
        r   = [r;P(m,m)];
    end;
    RMSPropDel  = [RMSPropDel,sqrt(r)];
    Fcore       = Fcore9(Lat,vENU,aENU);
    FSC2NE9x9   = SC2NE9x9(aSensedENU);
    PhiUL       = expm([Fcore,FSC2NE9x9;zeros(9),FSC9]);
    Phi         = [PhiUL,zeros(18,NumSats+2);zeros(2,18),Phiclock,zeros(2,NumSats);zeros(NumSats,20),PhiPR];
    P           = Phi*P*Phi' + Q;
end;
%
figure;
plot(time/3600,CEPGNSSINS,'k-','LineWidth',2);
text(1.5,.117,'GNSS','HorizontalAlignment','center','VerticalAlignment','middle','FontWeight','bold','FontSize',12);
text(1.5,.111,'OFF','HorizontalAlignment','center','VerticalAlignment','middle','FontWeight','bold','FontSize',12);
text(3.5,.117,'GNSS','HorizontalAlignment','center','VerticalAlignment','middle','FontWeight','bold','FontSize',12);
text(3.5,.111,'OFF','HorizontalAlignment','center','VerticalAlignment','middle','FontWeight','bold','FontSize',12);
text(5.5,.117,'GNSS','HorizontalAlignment','center','VerticalAlignment','middle','FontWeight','bold','FontSize',12);
text(5.5,.111,'OFF','HorizontalAlignment','center','VerticalAlignment','middle','FontWeight','bold','FontSize',12);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('CEP [NMi]','FontWeight','bold','FontSize',12);
title('INTEGRATED GNSS/INS ON 100 KM FIG-8 TRACK','FontWeight','bold','FontSize',12);
%
figure;
subplot(2,1,1),
plot(time/3600,HorGNSSINS,'k-','LineWidth',2);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('HORIZ. UNCERT. [m]','FontWeight','bold','FontSize',12);
title('INTEGRATED GNSS/INS ON 100 KM FIG-8 TRACK','FontWeight','bold','FontSize',12);
subplot(2,1,2),
plot(time/3600,AltGNSSINS,'k-','LineWidth',2);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('VERT. UNCERT. [m]','FontWeight','bold','FontSize',12);
%
figure;
plot(time/3600,VelGNSSINS,'k-','LineWidth',2);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('RMS VELOCITY UNCERTAINTY [m/s]','FontWeight','bold','FontSize',12);
title('INTEGRATED GNSS/INS ON 100 KM FIG-8 TRACK','FontWeight','bold','FontSize',12);
%
figure;
subplot(2,1,1),
plot(time/3600,1e3*TiltGNSSINS,'k-','LineWidth',2);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('RMS TILTS [m-rad]','FontWeight','bold','FontSize',12);
title('INTEGRATED GNSS/INS ON 100 KM FIG-8 TRACK','FontWeight','bold','FontSize',12);
subplot(2,1,2),
plot(time/3600,1e3*HeadGNSSINS,'k-','LineWidth',2);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('RMS HEADING [m-rad]','FontWeight','bold','FontSize',12);
%
figure;
subplot(2,1,1),
plot(time/3600,1e6*RMSAccComp(1:3,:)/9.8,'k-','LineWidth',2);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('ACC. BIAS [\mu G]','FontWeight','bold','FontSize',12);
title('INTEGRATED GNSS/INS ON 100 KM FIG-8 TRACK','FontWeight','bold','FontSize',12);
subplot(2,1,2),
plot(time/3600,1e6*RMSAccComp(4:6,:),'k-','LineWidth',2);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('ACC. S.F. [ppm]','FontWeight','bold','FontSize',12);
%
figure;
plot(time/3600,1e6*RMSGyroComp,'k-','LineWidth',2);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('RMS GYRO BIAS UNCERT. [\mu rad/s]','FontWeight','bold','FontSize',12);
title('INTEGRATED GNSS/INS ON 100 KM FIG-8 TRACK','FontWeight','bold','FontSize',12);
%
figure;
subplot(2,1,1),
plot(time/3600,RMSclock(1,:),'k-','LineWidth',2);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('CLOCK BIAS [m]','FontWeight','bold','FontSize',12);
title('INTEGRATED GNSS/INS ON 100 KM FIG-8 TRACK','FontWeight','bold','FontSize',12);
subplot(2,1,2),
plot(time/3600,RMSclock(2,:),'k-','LineWidth',2);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('CLOCK DRIFT [m/s]','FontWeight','bold','FontSize',12);
%
figure;
subplot(2,1,1);
plot(time/3600,RMSPropDel,'k-','LineWidth',1.5);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('\sigma PROP. DEL. [m]','FontWeight','bold','FontSize',12);
title('INTEGRATED GNSS/INS ON 100 KM FIG-8 TRACK','FontWeight','bold','FontSize',12);
subplot(2,1,2);
plot(time/3600,NoSatsUsed,'k-','LineWidth',1.5);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',12);
ylabel('# SATs USED','FontWeight','bold','FontSize',12);
% NoSatsUsed

