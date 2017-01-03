%
% Evaluation Track 2 tracker dilution of GPS information
% Corrections made by H. B. Hablani, Department of Aerospace Engineering,
% Indian Institute of Technology Bombay
%
clear all;
close all;
global RA PA
YUMAdata;
Deltat  = 1;
s15     = 0.25881904510252;
P0      = [100*eye(3),zeros(3);zeros(3),eye(3)];
Phi     = [eye(3),Deltat*eye(3);zeros(3),eye(3)];
k       = 0;
R       = 1;  % 1 meter RMS pseudorange error
Lat = 40*pi/180;
Lon = 0;
Alt = 0;
for log10acc=-3:2,
    k = k + 1;
    Gs(k) = 10^log10acc;
    sigmaacc = 9.8*Gs(k);
%   Q = [zeros(3),zeros(3);zeros(3),(sigmaacc*Deltat)^2*eye(3)];
    Q = (sigmaacc*Deltat)^2*[Deltat^2/3*eye(3),Deltat.2*eye(3);Deltat.2*eye(3),eye(3)];
    P = P0;
    m = 0;
    for t=0:3600,
        m    = m + 1;
        Time(m) = t;
        HGPS = [HSatSim(t,Lat,Lon,Alt),zeros(29,3)];
        for j=1:29,
            if HGPS(j,3) <= -0.25881904510252 % 15-degrees above horizon
                P = P - P*HGPS(j,:)'/(HGPS(j,:)*P*HGPS(j,:)'+R)*HGPS(j,:)*P;
%               P = Phi*P*Phi' + Q; % code correction by H. B. Hablani
                P = (P+P')/2;
            end;
        end;
                P = Phi*P*Phi' + Q; % code correction by H. B. Hablani
                P = (P+P')/2;       % 30 May, 2011
        RPSpos(k,m) = sqrt((P(1,1)+P(2,2)+P(3,3))/3);
    end;
end;
figure;
semilogy(Time,RPSpos','k-');
xlabel('Time [sec]');
ylabel('Position Uncertainty RMS/axis [m]');
title('Type 2 Tracker Performance for Various Acceleration Levels');
%legend('10^{-3} G','10^{-2} G','10^{-1} G','10^{0} G','10^{1} G','10^{2} G');