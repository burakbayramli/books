% DampingTestF10part2
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
%  This is a test of the 9-state dynamic model for INS errors, to see how
%  simulated errors in horizontal velocity and tilt propagate over time.
%
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
Dt = 100;  % Intersample interval [s]
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
RMSerr = [10;10;2;.1;.1;.1;.0001;.0001;.001;2];
MSerr  = RMSerr.^2;
P = diag(MSerr);  % Initial covariance matrix of uncertainty
%
RMS = zeros(rows,1);
%
t = 0;
hours   = 3;
minutes = 60*hours;
seconds = 60*minutes;
%
% Initial error state
%
xi = zeros(rows,1);
xi(4) = 0.1;      % 0.1 m/s E velocity error
xihist = xi;
for time = Dt:Dt:seconds,
    t = [t,time];
    xi = Phi*xi;
    xihist = [xihist,xi];
    P = Phi*P*Phi' + Q;
    for n = 1:rows,
        RMS(n) = sqrt(P(n,n));
    end;
    RMSerr = [RMSerr,RMS];
    t = [t,time];
    PHT = P*Halt';
    K   = PHT/(Halt*PHT + R);
    %xi  = K*xi(10);
    xihist = [xihist,xi];
    P   = P - K*PHT';
    for n = 1:rows,
        RMS(n) = sqrt(P(n,n));
    end;
    RMSerr = [RMSerr,RMS];
end;
subplot(2,2,1),plot(xihist(1,:),xihist(2,:),'k-','LineWidth',1.5);
axis equal;
xlabel('EAST ERROR [m]');
ylabel('NORTH ERROR [m]');
title('ALTIMETER DAMPING ONLY');
subplot(2,2,2),plot(xihist(4,:),xihist(5,:),'k-','LineWidth',1.5);
axis equal;
xlabel('EAST VEL. [m/S]');
ylabel('NORTH VEL. [m/S]');
title('0.1 m/s EAST VEL. ERR.');
subplot(2,2,3),plot(xihist(7,:)*1000,xihist(8,:)*1000,'k-','LineWidth',1.5);
axis equal;
xlabel('EAST TILT [mrad]');
ylabel('NORTH TILT [mrad]');
title('NO ACC/GYRO NOISE');
subplot(2,2,4),plot(t/3600,RMSerr(3,:),'k-','LineWidth',1.5);
xlabel('TIME IN HOURS');
ylabel('RMS ALT. ERR. [m]');
title('NO HORIZONTAL DAMPING');
