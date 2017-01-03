%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
%
% AccBiasCarousel.m
% Evaluates effect of gimbal INS accelerometer biases on navigation error,
% with and without carouseling.
%
clear all;
close all;
OmegaEarth   = 7292115167e-14; %WGS84 model
OmegaSchuler = 2*pi/84.4/60;
DeltaT       = 1; % intersample interval
phi          = pi/4; % latitude
AlphaDot     = 2*pi/5/60; % five minute carousel period
DeltaAlpha   = DeltaT*AlphaDot;
%
% dynamic coefficient matrix and state transition matrix
%
F = [0, 0, 1, 0; 0, 0, 0, 1; -OmegaSchuler^2, 0, 0, 2*OmegaEarth*sin(phi); 0, -OmegaSchuler^2, -2*OmegaEarth*sin(phi), 0];
Phi = expm(DeltaT*F);
%
% North accelerometer bias = 10 micro-g 
%
MicroGbias = 10;
b0 = [0;9.8*MicroGbias*1e-6];
%
% Initial errors = 0
%
x0     = zeros(4,1); % Gimbaled ENU IMU
xC     = zeros(4,1); % Carouseled IMU
Alpha  = 0;
%
%
%
hours = 14;
E0(1) = x0(1);
N0(1) = x0(2);
EC(1) = xC(1);
NC(1) = xC(2);
Time(1) = 0;
k = 1;
for t=DeltaT:DeltaT:hours*3600;
    k = k + 1;
    Time(k) = t;
    Alpha   = Alpha + DeltaAlpha;
    if Alpha > pi
        Alpha - Alpha - 2*pi;
    end;
    cA      = cos(Alpha);
    sA      = sin(Alpha);
    RM      = [cA,sA;-sA,cA];
    bC      = RM*b0;
    x0      = Phi*x0 + [zeros(2,1);b0*DeltaT];
    xC      = Phi*xC + [zeros(2,1);bC*DeltaT];
    E0(k)   = x0(1);
    N0(k)   = x0(2);
    EC(k)   = xC(1);
    NC(k)   = xC(2);
end;
figure;
plot(E0,N0,'r-',EC,NC,'b-');
title([num2str(hours),' hours simulated gimbaled INS errors from ',num2str(MicroGbias),' micro-g N acc. bias']);
xlabel('East position error [meter]');
ylabel('North position error [meter]');
legend('Stationary INS','Carouseled INS');
axis equal;
