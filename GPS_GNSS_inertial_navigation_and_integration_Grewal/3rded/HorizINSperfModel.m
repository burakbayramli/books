%
% A single-parameter model of horizontal INS errors is used to characterize
% CEP rate as a function of that parameter.
% 
%
clear all;
close all;
%
dt              = 1;        % time-sep
g               = 9.8;      % gravitational acceleration [m/s/s]
OmegaEarth      = 7.3e-5;   % earthrate [rad/sec]
OmegaSchuler    = 0.00124;  % Schuler frequency [rad/sec]
phi             = 40*pi/180;% latitude [rad]
%
% Dynamic coefficient matrix (FINS) and state transition matrix (PhiINS)
% for the 6-state INS horizontal error model.
%
FINS   = [0 0 1 0 0 0; 0 0 0 1 0 0; -OmegaSchuler^2 0 0 -2*sin(phi)*OmegaEarth -g 0; 0 -OmegaSchuler^2 2*sin(phi)*OmegaEarth 0 0 g; 0 0 0 0 0 0; 0 0 0 0 0 0];
PhiINS = expm(dt*FINS);
%
% Compute CEP rate as a function of the model parameter q
%
j = 0;
for log10q=-24:2,
    j       = j + 1;
    Log10q(j) = log10q;
    q(j)    = 10^log10q;
    Q       = zeros(6);
    Q(5,5)  = q(j);
    Q(6,6)  = q(j);
    P       = zeros(6);
    k       = 0;
    for t=0:dt:3600;
        k           = k + 1;
        Hr(k)       = t/3600;
        NMiCEP(j,k) = 1.177410022*sqrt(P(1,1)+P(2,2))/1852;
        %
        % There are 1852 meters per nautical mile.
        % The factor 1.177410022 for converting radial RMS to CEP
        % is exact for circular normal distributions.
        %
        P           = PhiINS*P*PhiINS' + Q;
    end;
end;
%
% CEP rate is computed from least-squares straight-line fit of CEP versus
% time over 1 hour period.
%
CEPrate = Hr*NMiCEP'/(Hr*Hr'); % least squares solution for CEP rate
%
% Plot CEP rate versus q
%
loglog(q,CEPrate,'k-');
xlabel('q-parameter [rad^2/sec]');
ylabel('CEP Rate [NMi/Hr]');
title('Single-parameter Model for INS Performance');
%
% Compute & display q-values for CEP rate= .01, .1, 1, 10, 100, 1000 NMi/Hr
%
j = 0;
for log10CEPrateX=-2:3,
    j = j + 1;
    CEPrateX(j) = 10^log10CEPrateX;
    qX(j)       = 10^interp1(log10(CEPrate),log10(q),log10CEPrateX);
    disp(['CEP Rate = ',num2str(CEPrateX(j)),'[NMi/Hr], q = ',num2str(qX(j))]);
end;
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
