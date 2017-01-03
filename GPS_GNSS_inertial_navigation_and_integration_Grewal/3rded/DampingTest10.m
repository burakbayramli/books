% DampingTest10
%  
%  Test altimeter damping of INS, using 9-state modelof INS errors
%
%  TEST CONDITIONS:
%
%    ALTIMETER IS ONLY SENSOR
%       VERTICAL CHANNEL KALMAN FILTER DAMPING
%    NO HORIZONTAL CONSTRAINTS
%    NO MEASURABLE HORIZONTAL ACCELERATION
%    NO SENSOR NOISE
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
rmsAlt = 20; % Long-term RMS atimeter error [m]
%
Qalt   = rmsAlt*Dt/tauAlt;
%
rmsAltNoise = 1; % Short-term RMSaltimeter noise [m]
%
R = rmsAltNoise^2; % Mean-squared altimeter reading noise
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
P = diag(MSerr);  % Initial covariance matrix of uncertainty
%
RMS = zeros(rows,1);
%
t = 0;
hours   = 3;
minutes = 60*hours;
seconds = 60*minutes;
for time = Dt:Dt:seconds,
    t = [t,time];
    P = Phi*P*Phi' + Q;
    for n = 1:rows,
        RMS(n) = sqrt(P(n,n));
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
end;
subplot(2,2,1),plot(t/3600,RMSerr(1,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS EAST ERR. [m]');
title('ALTIMETER DAMPING TEST');
subplot(2,2,2),plot(t/3600,RMSerr(2,:),'k-');
xlabel('TIME IN HOURS');
ylabel('RMS NORTH ERR. [m]');
title('NO HORIZONTAL DAMPING');
subplot(2,2,3),plot(t/3600,RMSerr(3,:),'k-');
xlabel('TIME IN HOURS');
ylabel('RMS ALT. ERR. [m]');
subplot(2,2,4),plot(t/3600,RMSerr(6,:),'k-');
xlabel('TIME IN HOURS');
ylabel('RMS VERT. VEL. ERR. [m/s]');
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
