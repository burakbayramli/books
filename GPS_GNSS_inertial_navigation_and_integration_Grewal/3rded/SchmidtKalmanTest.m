%
% Comparisons between Schmidt-Kalman filters and Kalman filters for
% GPS navigation with time-correlated pseudorange errors.
%
clear all;
close all;
global RA PA
YUMAdata;         % 08 March 2006 almanac data with 29 satellites
NoSats  = 29;     % Number Of Satellites
Dt      = 1;      % time interval between GPS pseudoranges
s15     = 0.25881904510252; % sine of 15 degrees (for selecting Sats)
sigma0  = 20*sqrt(3);   % initial RMS position uncertainty (radial)
sigmav  = 200;    % RMS velocity (about 447 mph RMS speed, radial)
sigmaa  = .5*9.8; % RMS acceleration (1/2 G radial)
sigmap  = 10;     % RMS pseudorange uncertainty (steady-state)
sigmaMN = 10;     % RMS pseudorange measurement noise (white, zero-mean)
taup    = 150;     % correlation time-constant of pseudorange errors
taua    = 120;    % correlation time-constant of acceleration
sigmaPR = sigmap*sqrt(1 - exp(-2*Dt/taup))/Dt;
[tauv,rhovelacc,sigmajerk] = DAMP2ParamsC(sigmaa,sigmav,taua,Dt);
%
% Initial covariance of uncertainty
%
Pee     = [(1/3)*sigma0^2*eye(3),zeros(3,6);zeros(3),sigmav^2/3*eye(3),rhovelacc*sigmav*sigmaa*eye(3)/3;zeros(3),rhovelacc*sigmav*sigmaa*eye(3)/3,sigmaa^2/3*eye(3)];
Pen     = zeros(9,NoSats);
Pnn     = sigmap^2*eye(NoSats);
P       = [Pee ,zeros(9,NoSats);zeros(NoSats,9),Pnn ];
%
% State transition matrix (constant)
%
ev      = exp(-Dt/tauv); % velocity
ea      = exp(-Dt/taua); % acceleration
ep      = exp(-Dt/taup); % pseudorange
Phie    = [eye(3),Dt*eye(3),.5*Dt^2*eye(3);
           zeros(3),ev*eye(3),Dt*eye(3);
           zeros(3),zeros(3),ea*eye(3)];
Phin    = [ep*eye(NoSats)];
Phi     = [Phie,zeros(9,NoSats);
           zeros(NoSats,9),Phin];
%
% Covariance of dynamic disturbance noise
%
Qee     = [zeros(6,9);zeros(3,6),sigmajerk^2*Dt^2/3*eye(3)];
Qnn     = sigmaPR^2*Dt^2*eye(NoSats);
Q       = [Qee,zeros(9,NoSats);zeros(NoSats,9),Qnn];
%
% Measurement noise covariance
%
R       = sigmaMN^2*eye(NoSats);
%
%
%
Lat     = 40*pi/180;
Lon     = 0;
Alt     = 100;
k       = 0;
count   = 0;
SS      = 0;
NoSecs  = 60*60*4; % Four hours of simulation
for t=0:NoSecs,
    k = k + 1;
    Time(k) = t;
    HGPS = [HSatSim(t,Lat,Lon,Alt),zeros(NoSats,6),eye(29)];
    NoSatsAvail = 29;
    %
    % Zero out the sensitivities to satellites below 15 degrees
    %
    for j=1:29,
        if HGPS(j,3) > -0.25881904510252 %  < 15-degrees above horizon
            NoSatsAvail = NoSatsAvail - 1;
            HGPS(j,1) = 0;
            HGPS(j,2) = 0;
            HGPS(j,3) = 0;
        end;
    end;
    NoSatsInView(k) = NoSatsAvail;
    He = HGPS(:,1:9);
    Hn = HGPS(:,10:38);
    %
    % Kalman filter
    %
    HP = HGPS*P;
    P = P - (HP'/(HP*HGPS' + R))*HP;
    %asym(k) = max(max(abs((P-P')/2))); % asymmetry test
    P = (P+P')/2;
    P = Phi*P*Phi' + Q;
    P = (P+P')/2;
    KalSq  = (P(1,1)+P(2,2)+P(3,3))/3;
    Kalman(k) = sqrt(KalSq);
    if k >= 10
        SS        = SS + KalSq;
        count  = count + 1;
    end;
    %
    % Schmidt-Kalman filter
    %
    [Pee,Pen,Pnn] = ObsCovarSK(Pee,Pen,Pnn,He,Hn,R);
    [Pee,Pen,Pnn] = TemporalCovarSK(Pee,Pen,Pnn,Phie,Phin,Qee,Qnn);
    Schmidt(k) = sqrt((Pee(1,1)+Pee(2,2)+Pee(3,3))/3);
end;
RMSKal  = sqrt(SS/count);
figure;
Nsecs = length(Time);
RMSDiff = sqrt(abs(Schmidt(10:Nsecs).^2-Kalman(10:Nsecs).^2));
subplot(2,1,1),
plot(Time(10:Nsecs)/3600,RMSDiff,'k-','LineWidth',1.5);
text(2,6,['{\sigma}_{Kalman} = ',num2str(RMSKal),' [m]'],'FontSize',15);
title('Difference between Schmidt-Kalman Filter and Kalman Filter RMS Error/Axis','FontSize',15);
xlabel('Time [hr]','FontSize',15);
ylabel('Difference [m]','FontSize',15);
subplot(2,1,2),
plot(9/3600,0,'w.',NoSecs/3600,12,'w.',Time(10:Nsecs)/3600,NoSatsInView(10:Nsecs),'k-','LineWidth',1.5);
%title([num2str(NoSecs/60/60),' Hrs of Simulated GPS Operation']);
xlabel('Time [hr]','FontSize',15);
ylabel('No. Satellites Used','FontSize',15);
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  

