%
% Plot estimated CEP rate as a function of accelerometer and gyro sensor
% compensation errors, using Fcore7 (horizontal-only) model for INS error
% propagation, plus process noise covariances for comensation parameters
% only.  NO SENSOR NOISE
%
%
% Grewal, Andrews, and Bartone,
% Global Navigation Satellite Systems, Inertial Navigation, and Integration
% 3rd Edition, Wiley-Interscience, 2012
%
clear all;
close all;
LatDeg       = 39.7931;         % Approximate location of 
LonDeg       = -86.2389;        % Indianapolis Motor Speedway
Altitude     = 221;             %
LatRad       = LatDeg*pi/180;
LonRad       = LonDeg*pi/180;
Dt = 10;                    %  DISCRETE TIME STEP [s]
hours   = 6;                % TEST RUN-TIME IN HOURS
minutes = 60*hours;         % TEST RUN-TIME IN MINUTES
seconds = 60*minutes;       % TEST RUN-TIME IN SECONDS
k = 0;
bestyet = 1e6;              % THRESHOLD FOR PLOTTING CEP HISTORY
figure;                     %   OF SAMPLE WITH CEPrate CLOSEST TO 1
taua    = 60*60;            % Accelerometer comp. err. correlation time [s]
% 
% Dynamic coefficient matrix for sensor compensation errors
% 
FSC     = [-1/taua*eye(4),zeros(4,3);zeros(3,7)]; 
% PhiSC   = expm(dt*FSC);
%
firsttime = 1;
for log10sigmaab = -5:.1:0,  % 6 ORDER-OF-MAGNITUDE RANGE, RMS BIAS ERR.
    k    = k + 1;
    logsigmaab(k) = log10sigmaab;
    RMSab    = 10^(log10sigmaab);
    Qab      = 2*10^(2*log10sigmaab)/taua;
    j    = 0;
    Qab  = 2*10^(2*log10sigmaab)/taua;
    for log10sigmaas = -3:.1:2,% 6 ORDER-OF MAGNITUDE RANGE, RMS S.F. ERR.
        j        = j + 1;
        logsigmaas(j) = log10sigmaas;
        RMSas    = 10^(log10sigmaas);
        Qas      = 2*10^(2*log10sigmaas)/taua;
        %
        % Initial covariance of uncertainty in 14 state variables
        %   1 m RMS position (2) [m]
        %   .01 m/s RMS velocity (2) [m/s]
        %   1e-6 RMS misalignment errors (3) [rad]
        %   variable RMS accelerometer biases (2) [m/s/s]
        %   variable RMS accelerometer scale factors (2) [unitless]
        %   zero RMS gyro biases (3) [rad/s]
        %
        P = zeros(14,14);
        Q = [zeros(7,14);zeros(2,7),Qab*eye(2),zeros(2,5);zeros(2,9),Qas*eye(2),zeros(2,3);zeros(3,14)];
        Hr         = [];
        CEPvHr     = [];
        if firsttime,
            CEP0vHr    = [];
        end;
        %
        %                % VARIABLES FOR LEAST-SQUARES FIT OF CEPrate
        %
        Nsamples   = 0;  % CEP data index (ending value will be data record length)
        sumhr      = 0;  % cumsum of time-since-start in hours
        sumhrsq    = 0;  % cumsum of (time-since-start)-squared in hours
        sumCEP     = 0;  % cumsum of CEP values computed from data record
        sumhrCEP   = 0;  % cumsum of product of CEP and (time-since-start)
        if firsttime,
            sumCEP0    = 0;  % cumsum of CEP values with INS turned off
            sumhrCEP0  = 0;  % cumsum of product of CEP and (time-since-start)
        %                    % with INS turned off
        end;
        %
        for time = 0:Dt:seconds,
            Nsamples  = Nsamples + 1;
            hr        = time/3600;
            Hr        = [Hr,hr];
            [xENU,vENU,aSensedENU,omegaSensedENU] = Fig8TrackSimENU(time,LatRad,LonRad);
            if firsttime,
                CEP0      = 1.2*sqrt(xENU(1)^2 + xENU(2)^2)/1852; % error w/o INS
                CEP0vHr   = [CEP0vHr,CEP0];
            end;
            Fcore     = Fcore7(LatDeg,vENU);
            FSC2NE7x7 = SC2NE7x7(aSensedENU);
            F         = [Fcore,FSC2NE7x7;zeros(7,7),FSC];
            Phi       = expm(Dt*F);
            P         = Phi*P*Phi' + Q;
            P         = .5*(P+P');  %  SYMMETRIZING P (GOOD IDEA)
            %
            % CEP IS APPROXIMATELY 1.2 TIMES RMS RADIAL POSITION ERROR
            %
            CEP       = 1.2*sqrt(P(1,1)+P(2,2))/1852;
            %
            %  ACCUMULATE DATA FOR LEAST-SQUARES FIT
            %
            CEPvHr    = [CEPvHr,CEP];
            sumhr     = sumhr + hr;
            sumhrsq   = sumhrsq + hr^2;
            sumCEP    = sumCEP + CEP;
            sumhrCEP  = sumhrCEP + hr*CEP;
            if firsttime,
                sumCEP    = sumCEP + CEP;
                sumhrCEP  = sumhrCEP + hr*CEP;
            end
        end;
        firsttime = 0;
        %
        %  LEAST-SQUARES ESTIMATE OF CEP RATE AND INTERCEPT
        %
        ATCEP   = [sumCEP;sumhrCEP];
        ATA     = [Nsamples,sumhr;sumhr,sumhrsq];
        LSfit   = ATA\ATCEP;                  % least-squares fit for:
        CEPinit = LSfit(1);                   % least-squares intercept
        CEPrate = LSfit(2);                   % CEPrate in NMi/hr
        CEPslope = sumhrCEP/sumhrsq;          % LS fit slope through [0,0]
        %
        % CHECK FOR CEPslope CLOSER TO 1
        %
        if abs(CEPslope - 1) < bestyet,
            bestyet = abs(CEPslope - 1);
            %
            % IF SO, PLOT CEP VERSUS TIME
            %
            plot(Hr,CEPvHr,'k-','LineWidth',2);
            xlabel('TIME [Hr]','FontWeight','bold','FontSize',14),
            ylabel('CEP [NMi]','FontWeight','bold','FontSize',14);
            title(['CEPslope = ',num2str(CEPslope),' [NMi/Hr]'],'FontWeight','bold','FontSize',14);
            % disp(['CEPrate = ',num2str(CEPrate),' [NMi/Hr], RMS Acc Bias = 1e',num2str(log10sigmaab),' [m/s^2], RMS Acc S.F. = 1e',num2str(log10sigmaas),' [unitless]']);
        end;
        logCEPrate(k,j)  = log10(CEPrate);
        logCEPinit(k,j)  = log10(CEPinit);
        logCEPslope(k,j) = log10(CEPslope);
        disp(['RMS Acc Bias = 1e',num2str(log10sigmaab),' [m/s^2], RMS Acc S.F. = 1e',num2str(log10sigmaas),', CEPrate = ',num2str(CEPrate),' [NMi/Hr]']);
        disp(['RMS Acc Bias = 1e',num2str(log10sigmaab),' [m/s^2], RMS Acc S.F. = 1e',num2str(log10sigmaas),', CEPslope = ',num2str(CEPslope),' [NMi/Hr]']);
    end;
end;
%
% compute CEP rate for INS turned off
%
ATCEP   = [sumCEP0;sumhrCEP0]; 
ATA     = [Nsamples,sumhr;sumhr,sumhrsq];
LSfit   = ATA\ATCEP;                  % least-squares fit for:
CEP0rate = LSfit(2);                  % CEPrate in NMi/hr
figure;
plot(Hr,CEP0vHr,'k-','LineWidth',.2);
xlabel('TIME [Hr]','FontWeight','bold','FontSize',14),
ylabel('CEP [NMi]','FontWeight','bold','FontSize',14);
title(['CEPrate w/o Navigation = ',num2str(CEP0rate),' [NMi/Hr]'],'FontWeight','bold','FontSize',14);
disp(['CEP rates range from ',num2str(10^min(min(logCEPrate))),' [NMi/Hr] to ',num2str(10^max(max(logCEPrate))),' [NMi/Hr]']);
%
%  GENERATE CONTOUR PLOT OF CEPrate AT POWERS OF 10 [NMi/Hr]
%  VERSUS RMS ACCELEROMETER BIAS AND SCALE FACTOR ERRORS AT POWERS OF 10
%
figure;
%contour(logCEPrate,logQgyro,logQacc,(-6:6),'k-','LineWidth',2);
contour(logCEPrate,(-6:6),'k-','LineWidth',2); % w/o X-Y units
%contour(interp2(logCEPrate,4),(-6:6),'k-','LineWidth',2); % Interpolated
%title('CEP Rate versus Accelerometer & Gyro Noise','FontWeight','bold','FontSize',14);
figure;
contour(logCEPinit,(-6:6),'k-','LineWidth',2); % w/o X-Y units
disp(['CEP intercepts range from ',num2str(10^min(min(logCEPinit))),' [NMi] to ',num2str(10^max(max(logCEPinit))),' [NMi]']);
figure;
contour(logCEPslope,(-6:6),'k-','LineWidth',2); % w/o X-Y units
disp(['CEP slopes range from ',num2str(10^min(min(logCEPslope))),' [NMi/Hr] to ',num2str(10^max(max(logCEPslope))),' [NMi/Hr]']);
