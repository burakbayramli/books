% Fcore10Test1
%
% Grewal, Andrews, and Bartone,
% Global Navigation Satellite Systems, Inertial Navigation, and Integration
% 3rd Edition, Wiley, 2012
%
%  
%  Test barometric altimeter damping of INS, using Fcore9 model of INS
%  errors, plus model for altimeter and ambient barometric pressure
%  veriation.
%
%  TEST CONDITIONS:
%
%    ALTIMETER IS ONLY SENSOR
%       VERTICAL CHANNEL KALMAN FILTER DAMPING
%    NO HORIZONTAL CONSTRAINTS
%    NO MEASURABLE HORIZONTAL ACCELERATION
%    NO SENSOR NOISE
%  
%  OUTPUT PLOTS:
%    
%    RMS EASTING ERROR VS TIME IN HR
%    RMS NORTHING ERROR VS TIME IN HR
%    RMS ALTITUDE ERROR VS TIME IN HR
%    RMS VERTICAL VELOCITY ERROR VS TIME IN HR
%
%    RMS NORTH VEL. ERROR VS  RMS EAST VEL. ERROR
%    RMS NORTH TILT ERROR VS  RMS EAST TILT ERROR
%    RMS HEADING ERROR VS TIME IN HR
%    RMS UNCERTAINTY IN ALTIMETER BIAS ERROR VS TIME IN HR
%
%    CEP VERSUS TIME IN HR, WITH LEAST-SQUARES-FIT CEP RATE IN NMI/HR
%
clear all;
close all;
%  
Dt = 10;  % Intersample interval [s]
%  
Lat = 40; % Test latitude
%  
vENU = zeros(3,1); % Test velocity
%
tauAlt = 24*60*60; % Altimeter bias correlation time [s]
%
rmsAlt = 100; % Long-term RMS atimeter error [m]
%
Qalt   = rmsAlt^2*Dt/tauAlt;
%
rmsAltNoise = 1; % Short-term RMSaltimeter noise [m/root-sec]
%
R = rmsAltNoise^2/Dt; % Mean-squared altimeter reading noise
%
Halt = [0,0,1,0,0,0,0,0,0,1]; % Measurement sentivity matrix
%
F = Fcore10(Lat,vENU,tauAlt); % Dynamic coefficient matrix
[rows,cols] = size(F);
Phi = expm(Dt*F);             % State transition matrix
%
Q = diag([0,0,0,0,0,0,0,0,0,Qalt]); % Process noise covariance
%
% Initial RMS and mean-squared state uncertainties
%
RMSerr = [10;10;5;.05;.05;.01;.00001;.00001;.0001;5];  
MSerr  = RMSerr.^2;
P = diag(MSerr);     % Initial covariance matrix of navigation uncertainty
%
%RMS = zeros(rows,1);
%
t = 0;                 % test starting time in seconds
hours   = 6;           % test run-time in hours
minutes = 60*hours;    % test run-time in minutes
seconds = 60*minutes;  % test run-time in seconds
%
% TIME t sampled at intervals of Dt seconds
%
for time = Dt:Dt:seconds,
    t = [t,time];
    P = Phi*P*Phi' + Q;
    for n = 1:rows,
        RMS(n,1) = sqrt(P(n,n));
    end;
    RMSerr = [RMSerr,RMS];
    t = [t,time];
    PHT = P*Halt';
    K   = PHT/(Halt*PHT + R);
    P   = P - K*PHT';
    for n = 1:rows,
        RMS(n) = sqrt(P(n,n));
    end;
    RMSerr = [RMSerr,RMS];
    P   = .5*(P+P');
end;
%
% Time record t has N = hours*3600/Dt+1 entries, starting at t(1)=0
%
% Data record RMSerr is a 10xN matrix with rows equal to RMS uncertainties
%
subplot(2,2,1),plot(t/3600,RMSerr(1,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS EAST ERR. [m]');
title('Fcore9 MODEL + ALTIMETER');
subplot(2,2,2),plot(t/3600,RMSerr(2,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS NORTH ERR. [m]');
title('DAMPING TEST');
subplot(2,2,3),plot(t/3600,RMSerr(3,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS ALT. ERR. [m]');
title([num2str(rmsAltNoise,2),' m RMS ALTIMETER NOISE']);
subplot(2,2,4),plot(t/3600,RMSerr(6,:)*100,'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS VERT. VEL. ERR. [cm/s]');
title([num2str(rmsAltNoise,2),' m/s^{1/2} RMS PROCESS NOISE']);
%
figure;
subplot(2,2,1),plot(RMSerr(4,:),RMSerr(5,:),'k-','LineWidth',1.5);
xlabel('RMS E. VEL. [m/s]');
ylabel('RMS N. VEL. [m/s]');
title('Fcore9 MODEL + ALTIMETER');
subplot(2,2,2),plot(RMSerr(7,:)*1e6,RMSerr(8,:)*1e6,'k-','LineWidth',1.5);
xlabel('RMS E. TILT [ \muRad]');
ylabel('RMS N. TILT [ \muRad]');
title('DAMPING TEST');
subplot(2,2,3),plot(t/3600,RMSerr(9,:)*1e6,'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS HEADING ERR. [ \muRAD]');
title([num2str(rmsAltNoise,2),' m RMS ALTIMETER NOISE']);
subplot(2,2,4),plot(t/3600,RMSerr(10,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS ALTIMETER ERR. [m]');
title([num2str(rmsAltNoise,2),' m/s^{1/2} RMS PROCESS NOISE']);
%
% Least-squares fit of CEP over 0-3 hours in units of NMi/hr
%
% 1. Initialize the index (n) and the five summing variables
%
Dthr       = Dt/3600;
n          = 0;  % CEP data index (ending value will be data record length)
sumhr      = 0;  % cumsum of time-since-start in hours
sumhrsq    = 0;  % cumsum of (time-since-start)-squared in hours
sumCEP     = 0;  % cumsum of CEP values computed from data record
sumhrCEP   = 0;  % cumsum of product of CEP and (time-since-start)
%
% Set the starting time and run time of the least-squares fit
%
hrstart    = 0;                        % Starting time in hours
hrrun      = 3;                        % least-square fit run time [hr]
%
% Calculate the dependent indexing values
%
hrend      = hrstart + hrrun;          % Ending time in hours
kstart     = ceil(hrstart/Dthr*2) + 1; % Starting column in data matrix
kend       = ceil(hrend/Dthr*2) + 1;   % Ending column in data matrix
for k=kstart:kend,
    n         = n + 1;
    hr(n)     = t(k)/3600;
    tss       = hr(n) - hrstart;     % times-since-start in hours
    sumhr   = sumhr + tss;
    sumhrsq = sumhrsq + tss^2;
    CEP(n)    = 1.2*sqrt(RMSerr(1,k)^2 + RMSerr(2,k)^2)/1852; % CEP [NMi]
    sumCEP    = sumCEP + CEP(n);
    sumhrCEP  = sumhrCEP + CEP(n)*tss;
end;
%
% Least-squares fit to CEP0 (initial CEP in NMi) and CEPrate in NMi/hr
%
Ndpts   = n;                          % Number of data points used
ATCEP   = [sumCEP;sumhrCEP];
ATA     = [Ndpts,sumhr;sumhr,sumhrsq];
LSfit   = ATA\ATCEP;                  % least-squares fit for:
CEP0    = LSfit(1);                   % initial CEP at starting time, and
CEPrate = LSfit(2);                   % CEPrate in NMi/hr
%
figure;
plot(hrstart,0,'w.',hrstart,1,'w.',hr,CEP,'k-','LineWidth',1.5);
xlabel('TIME [hr]','FontWeight','bold','FontSize',12),
ylabel('CEP [NMi]','FontWeight','bold','FontSize',12);
title('Fcore9 MODEL + ALTIMETER: DAMPING TEST','FontWeight','bold','FontSize',12);
text(hrstart+.1*hrrun,.9,'LEAST-SQUARES FIT','FontWeight','bold','FontSize',12);
text(hrstart+.1*hrrun,.8,['CEP RATE = ',num2str(CEPrate),' [NMi/hr]'],'FontWeight','bold','FontSize',12);
