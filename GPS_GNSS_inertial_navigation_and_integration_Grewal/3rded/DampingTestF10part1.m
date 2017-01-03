% DampingTestF10part1
%
%  M. S. Grewal, A.P. Andrews, & C. G. Bartone, 
%  Global Navigation Satellite Systems, Intertial Navigation, and Integration, 
%  John Wiley & Sons, 2012.
%
%  Covariance analysis of altimeter damping vertical channel of INS,
%  using 9-state modelof INS errors plus altimeter with expoentially 
%  correlated bias with 20-meter RMS error with 24-hour correlation time,
%  plus uncorrelated noise of 1 meter.
%
%  This is a test using the 9-state dynamic model to show that an 
%  altimeter can, indeed, dampen INS altitude errors.  The recognition that
%  INS vertical errors were unstable was what led physicist George Gamow
%  (1904-1968) to challenge the military-funded inertial navigation 
%  development project directed by Charles Stark Draper at MIT in the
%  1940s.
%
%  TEST CONDITIONS:
%
%    POST-ALIGNMENT CONDITIONS (NO FURTHER INITIALIZATION/ALIGNMENT)
%    ALTIMETER IS ONLY SENSOR
%       VERTICAL CHANNEL KALMAN FILTER DAMPING
%    NO HORIZONTAL CONSTRAINTS
%    NO MEASURABLE HORIZONTAL ACCELERATION
%    NO ACCELEROMETER OR GYRO NOISE
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
tauAlt = 2*24*60*60; % 2-day altimeter bias correlation time [s]
%
rmsAlt = 150; % Long-term RMS atimeter error [m]
%
Qalt   = rmsAlt*Dt/tauAlt;
%
rmsAltNoise = 3; % Short-term RMSaltimeter noise [m]
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
RMSerr = [100;100;10;.1;.1;.1;.0001;.0001;.001;10];
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
subplot(2,2,1),plot(t/3600,RMSerr(3,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS ALT. ERR. [m]');
title('ALTIMETER DAMPING ONLY');
subplot(2,2,2),plot(t/3600,RMSerr(6,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS VERT. VEL. [m/s]');
title('POST-ALIGNMENT');
subplot(2,2,3),plot(t/3600,RMSerr(1,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS EAST ERR. [m]');
title('NO ACC/GYRO NOISE');
subplot(2,2,4),plot(t/3600,RMSerr(2,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS NORTH ERR. [m]');
title('NO HORIZONTAL DAMPING');
%
figure;
subplot(2,2,1),plot(t/3600,RMSerr(4,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('EAST VEL. [m/s]');
title('ALTIMETER DAMPING ONLY');
subplot(2,2,2),plot(t/3600,RMSerr(5,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('NORTH VEL. [m/s]');
title('POST-ALIGNMENT');
subplot(2,2,3),plot(t/3600,RMSerr(7,:)*1000,'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('EAST TILT [Mrad]');
title('NO ACC/GYRO NOISE');
subplot(2,2,4),plot(t/3600,RMSerr(8,:)*1000,'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('NORTH TILT [Mrad]');
title('NO HORIZONTAL DAMPING');
figure;
subplot(2,2,1),plot(t/3600,RMSerr(9,:)*1000,'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('HEADING [Mrad]');
title('ALTIMETER DAMPING ONLY');
subplot(2,2,2),plot(t/3600,RMSerr(10,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('ALIMETER BIAS [m]');
title('POST-ALIGNMENT');
%
figure;
plot(t/3600,RMSerr(3,:),'k-','LineWidth',1.5)
xlabel('TIME IN HOURS','FontWeight','bold','FontSize',12);
ylabel('RMS ALTITUDE ERROR [m]','FontWeight','bold','FontSize',12);
title('Fcore10 INS MODEL WITH ALTIMETER DAMPING ONLY','FontWeight','bold','FontSize',12);
