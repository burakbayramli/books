%
% Simulated INS vertical channel covariances with and without damping
%
clear all;
close all;
Raltimeter     = 10;      % RMS altimeter noise [meter]
SigmaAltitude  = 100;     % RMS altimeter altitude offset error [meter]
TauAltimeter   = 10*3600; % altimeter offset correlation time [10 hours]
DeltaT         = 10;      % sampling interval [1 minute]
TSchuler       = 5046;    % Schuler period [sec]
Qaltimeter     = (1-exp(-2*DeltaT/TauAltimeter))*SigmaAltitude^2;
%
% State transition matrices, non-augmented and augmented
%
Phi2           = .5*exp(DeltaT/TSchuler)*[1,TSchuler;1/TSchuler,1] + .5*exp(-DeltaT/TSchuler)*[1,-TSchuler;-1/TSchuler,1];
Phi3           = [Phi2,zeros(2,1);zeros(1,2),exp(-DeltaT/TauAltimeter)];
H              = [1,0,1];
Legends        = [];
figure;
semilogy(0,.1,'w.');
hold on;
hours = 1;  % Simulated hours of operation
for log10mpsprh=-2:2,
    mpsprh = 10^log10mpsprh; % meter/sec/root-hour accelerometer noise
    Legend = ['10^{',num2str(log10mpsprh),'} m/s/sqrt(hr)'];
    Qaccelerometer = mpsprh^2*DeltaT/3600;
    Q2             = [0,0;0,Qaccelerometer];
    Q3             = [Q2,zeros(2,1);zeros(1,2),Qaltimeter];
    P2             = [1,0;0,1e-4]; % 1 m alt. uncert., 10 mm/s vel. uncert.
    P3             = [P2,zeros(2,1);zeros(1,2),1];
    k              = 1;
    Time(1)        = 0;
    Sigma2(1)      = sqrt(P2(1,1));
    Sigma3(1)      = sqrt(P3(1,1));
    for t = DeltaT:DeltaT:3600*hours,
        k         = k + 1;
        Time(k)   = t/3600; % time in hours
        P2        = Phi2*P2*Phi2' + Q2;
        Sigma2(k) = sqrt(P2(1,1));
        P3        = Phi3*P3*Phi3' + Q3; % a priori
        K         = P3*H'/(H*P3*H'+ Raltimeter);
        P3        = P3 - K*H*P3; % a posteriori
        P3        = .5*(P3 + P3');
        Sigma3(k) = sqrt(P3(1,1));
    end;
    semilogy(Time,Sigma2,'k-',Time,Sigma3,'k:');
    text(Time(length(Time))-.01,Sigma2(length(Time)),Legend,'HorizontalAlignment','right','VerticalAlignment','bottom');
end;
hold off;
legend('Altimeter Damping','Undamped');
title('INS Vertical Channel Uncertainties for Various Accelerometer Noise Levels');
xlabel('INS Running Time from Initialization [hours]');
ylabel('RMS INS Altitude Uncertainty [meter]');
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
