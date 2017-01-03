%
% Plot estimated CEP rate as a function of accelerometer and gyro noise,
% using Fcore7 (horizontal-only) model for INS error propagation, plus
% process noise covariances.
%
%
% Grewal, Andrews, and Bartone,
% Global Navigation Satellite Systems, Inertial Navigation, and Integration
% 3rd Edition, Wiley, 2013
%
clear all;
close all;
Lat = 40;                   %  TEST LATITUDE (DEG)
vel = zeros(2,1);           %  INS STATIONARY
Dt = 10;                    %  DISCRETE TIME STEP [s]
Fcore = Fcore7(Lat,vel);    %  DYNAMIC COEFFICIENT MATRIX
Phi   = expm(Dt*Fcore);     %  STATE TRANSITION MATRIX
t       = 0;
hours   = 6;                % TEST RUN-TIME IN HOURS
minutes = 60*hours;         % TEST RUN-TIME IN MINUTES
seconds = 60*minutes;       % TEST RUN-TIME IN SECONDS
k = 0;
bestyet = 1e6;              % THRESHOLD FOR PLOTTING CEP HISTORY
figure;                     %   OF SAMPLE WITH CEPrate CLOSEST TO 1
for log10Qacc = -12:4,      % 16 ORDER-OF MAGNITUDE Qacc VALUES
    k    = k + 1;
    logQacc(k) = log10Qacc;
    j    = 0;
    Qacc = 10^log10Qacc;
    for log10Qgyro = -20:-4,% 16 ORDER-OF MAGNITUDE Qgyro VALUES
        j        = j + 1;
        logQgyro(j) = log10Qgyro;
        Qgyro    = 10^log10Qgyro;
        MeanSqNoise = Dt*[0,0,Qacc,Qacc,Qgyro,Qgyro,Qgyro];
        Q = diag(MeanSqNoise);    % Process noise covariance
        P = zeros(7,7);
        Hr         = 0;
        CEPvHr     = 0;
        %
        %                % VARIABLES FOR LEAST-SQUARES FIT OF CEPrate
        %
        Nsamples   = 1;  % CEP data index (ending value will be data record length)
        sumhr      = 0;  % cumsum of time-since-start in hours
        sumhrsq    = 0;  % cumsum of (time-since-start)-squared in hours
        sumCEP     = 0;  % cumsum of CEP values computed from data record
        sumhrCEP   = 0;  % cumsum of product of CEP and (time-since-start)
        %
        for time = Dt:Dt:seconds,
            Nsamples  = Nsamples + 1;
            hr        = time/3600;
            Hr        = [Hr,hr];
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
        end;
        %
        %  LEAST-SQUARES ESTIMATE OF CEP RATE AND INTERCEPT
        %
        ATCEP   = [sumCEP;sumhrCEP];
        ATA     = [Nsamples,sumhr;sumhr,sumhrsq];
        LSfit   = ATA\ATCEP;                  % least-squares fit for:
        CEPrate = LSfit(2);                   % CEPrate in NMi/hr
        %
        % CHECK FOR CEPrate CLOSER TO 1
        %
        if abs(CEPrate - 1) < bestyet,
            bestyet = abs(CEPrate - 1);
            %
            % IF SO, PLOT CEP VERSUS TIME
            %
            plot(Hr,CEPvHr,'k-','LineWidth',2);
            xlabel('TIME [Hr]','FontWeight','bold','FontSize',14),
            ylabel('CEP [NMi]','FontWeight','bold','FontSize',14);
            title(['CEPrate = ',num2str(CEPrate),' [NMi/Hr]'],'FontWeight','bold','FontSize',14);
            disp(['CEPrate = ',num2str(CEPrate),' [NMi/Hr], Acc Noise = 1e',num2str(log10Qacc/2),' [m/s^2/sqrt(s)], Gyro Noise = 1e',num2str(log10Qgyro/2),' [rad/s/sqrt(s)]']);
        end;
        logCEPrate(k,j) = log10(CEPrate);
        disp(['Qacc = ',num2str(Qacc),', Qgyro = ',num2str(Qgyro),', CEP rate = ',num2str(CEPrate),' [NMi/Hr]']);
    end;
end;
%
%  GENERATE CONTOUR PLOT OF CEPrate AT POWERS OF 10 [NMi/Hr]
%  VERSUS ACCELEROMETER AND GYRO NOISE AT POWERS OF 10
%
figure;
%contour(logCEPrate,logQgyro,logQacc,(-6:6),'k-','LineWidth',2);
contour(logCEPrate,(-6:6),'k-','LineWidth',2); % w/o X-Y units
%contour(interp2(logCEPrate,4),(-6:6),'k-','LineWidth',2); % Interpolated
%title('CEP Rate versus Accelerometer & Gyro Noise','FontWeight','bold','FontSize',14);
