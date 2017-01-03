%
% For an integrated GNSS/INS navigator on a 100-km figure-8 test track,
% calculate RMS position and valocity uncertainties
% as a function of accelerometer and gyro noise.
%
% Use 51-state Kalman filter model for GNSS/INS error propagation and
% estimation, with sensor process noise as the only error source.
%
%
% Grewal, Andrews, and Bartone,
% Global Navigation Satellite Systems, Inertial Navigation, and Integration
% 3rd Edition, Wiley, 2013
%
clear all;
close all;
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
Phiclock    = expm(Fclock*dt); % state transition matrix
k           = 0;
for log10Qacc = -22:4,      % 16 ORDER-OF MAGNITUDE Qacc VALUES
    k    = k + 1;
    logQacc(k) = log10Qacc;
    j         = 0;
    qAccNoise = 10^log10Qacc;
    %for log10Qgyro = -20:-4,% 16 ORDER-OF MAGNITUDE Qgyro VALUES
    for log10Qgyro = -24:2,% 16 ORDER-OF MAGNITUDE Qgyro VALUES
        j        = j + 1;
        logQgyro(j) = log10Qgyro;
        qGyroNoise  = 10^log10Qgyro;
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
        Qnoise          = [zeros(3,9);zeros(3),qAccNoise*eye(3),zeros(3);zeros(3,6),qGyroNoise*eye(3)];
        %
        % Sensor compensation error process noise covariance and initial
        % covariance
        %
        tauSensorComp   = 3600;     % 1-hour corelation time
        RMSAccBias      = 0;        % 
        RMSAccSF        = 0;       % 
        RMSGyroBias     = 0;       % 
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
        HorGNSSINS  = [];
        AltGNSSINS  = [];
        HVlGNSSINS  = [];
        VVlGNSSINS  = [];
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
            [xENU,vENU,aENU,aSensedENU,omegaENU,omegaSensedENU] = Big8TrackSimENU(t,LatRad,LonRad,Alt); 
            HGNSS       = HSatENU(t,LatRad+xENU(2)/REarth,LonRad,Alt);
            NoSatsAvail = NumSats;
            Hclock      = ones(NumSats,1); % clock error sensitivity
            HPR         = eye(NumSats);    % propagation delay sensitivity
            for i=1:NumSats,
                if HGNSS(i,3) > -0.25881904510252 
                    NoSatsAvail = NoSatsAvail - 1;
                    HGNSS(i,1)  = 0;
                    HGNSS(i,2)  = 0;
                    HGNSS(i,3)  = 0;
                    Hclock(i)   = 0;
                    HPR(i,i)    = 0;
                end;
            end;
            H           = [HGNSS,zeros(NumSats,15),Hclock,zeros(NumSats,1),HPR];
            PHT         = P*H';
            Denom       = H*PHT + RPR;
            K           = PHT/Denom;
            P           = P - K*PHT';
            P           = .5*(P + P');
            HorGNSSINS  = [HorGNSSINS,P(1,1)+P(2,2)];
            AltGNSSINS  = [AltGNSSINS,P(3,3)];
            HVlGNSSINS  = [HVlGNSSINS,P(4,4)+P(5,5)];
            VVlGNSSINS  = [VVlGNSSINS,P(6,6)];
            r           = [];
            for m=21:20+NumSats,
                r   = [r;P(m,m)];
            end;
            Fcore       = Fcore9(Lat,vENU,aENU);
            FSC2NE9x9   = SC2NE9x9(aSensedENU);
            PhiUL       = expm([Fcore,FSC2NE9x9;zeros(9),FSC9]);
            Phi         = [PhiUL,zeros(18,NumSats+2);zeros(2,18),Phiclock,zeros(2,NumSats);zeros(NumSats,20),PhiPR];
            P           = Phi*P*Phi' + Q;
        end;
        L             =length(HorGNSSINS);
        RMSHor(k,j) = sqrt(mean(HorGNSSINS(360:L)));
        RMSAlt(k,j) = sqrt(mean(AltGNSSINS(360:L)));
        log10HVl(k,j) = log10(mean(HVlGNSSINS(360:L)))/2;
        log10VVl(k,j) = log10(mean(VVlGNSSINS(360:L)))/2;
        disp(['k = ',num2str(k),', j = ',num2str(j)]);
    end;
end;
%
%  GENERATE CONTOUR PLOTS OF RMS POSITION AND VELOCITY ERROR AT POWERS OF 10 
%  VERSUS MEAN-SQUARED ACCELEROMETER AND GYRO NOISE AT POWERS OF 10
%
figure;
%contour(RMSHor,logQgyro,logQacc,(-6:6),'k-','LineWidth',2);
lowerlimit = ceil(min(min(RMSHor)));
upperlimit = floor(max(max(RMSHor)));
disp(['RMS horizontal position from ',num2str(lowerlimit),' to ',num2str(upperlimit)]);
contour(RMSHor,(5:5:20),'k-','LineWidth',2); % w/o X-Y units
title('Horiz. Pos. Err. vs Acc. & Gyro Noise','FontWeight','bold','FontSize',14);
%
%
figure;
%contour(RMSAlt,logQgyro,logQacc,(-6:6),'k-','LineWidth',2);
lowerlimit = ceil(min(min(RMSAlt)));
upperlimit = floor(max(max(RMSAlt)));
disp(['RMS vertical position from ',num2str(lowerlimit),' to ',num2str(upperlimit)]);
contour(RMSAlt,(5:5:20),'k-','LineWidth',2); % w/o X-Y units
title('Vert. Pos. Err. vs Acc. & Gyro Noise','FontWeight','bold','FontSize',14);
%
%
figure;
%contour(log10HVl,logQgyro,logQacc,(-6:6),'k-','LineWidth',2);
lowerlimit = ceil(min(min(log10HVl)));
upperlimit = floor(max(max(log10HVl)));
disp(['RMS horizontal velocity from 10^',num2str(lowerlimit),' to 10^',num2str(upperlimit)]);
contour(log10HVl,(lowerlimit:upperlimit),'k-','LineWidth',2); % w/o X-Y units
title('Horiz. Vel. Err. vs Acc. & Gyro Noise','FontWeight','bold','FontSize',14);
%
%
figure;
%contour(log10VVl,logQgyro,logQacc,(-6:6),'k-','LineWidth',2);
lowerlimit = ceil(min(min(log10VVl)));
upperlimit = floor(max(max(log10VVl)));
disp(['RMS vertical velocity from 10^',num2str(lowerlimit),' to 10^',num2str(upperlimit)]);
contour(log10VVl,(lowerlimit:upperlimit),'k-','LineWidth',2); % w/o X-Y units
title('Vert. Vel. Err. vs Acc. & Gyro Noise','FontWeight','bold','FontSize',14);
%
logQgyro,
logQacc,