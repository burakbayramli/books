% Check RotVec2Quat and Quat2RotVec
% 
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
%
clear all;
close all;
%
nsamples = 10000;
for k=1:nsamples,
    rho0 = randn(3,1);
    %
    % 1. Convert rotation vector modulo 2*Pi to be of length <= pi
    %
    thetaRaw    = sqrt(rho0(1)^2+rho0(2)^2+rho0(3)^2);
    thetaMod2Pi = mod(thetaRaw,2*pi);
    if thetaMod2Pi>pi 
        thetaMod2Pi=thetaMod2Pi-2*pi; 
    end;
    RotVecIn    = (thetaMod2Pi/thetaRaw)*rho0;
    Quat        = RotVec2Quat(RotVecIn);
    RotVecOut   = Quat2RotVec(Quat);
    z(k)        = sqrt(sum((RotVecOut-RotVecIn).^2))/2/pi;
    if z(k) > pi
        z(k)    = sqrt(sum((RotVecOut+RotVecIn).^2))/2/pi;
    end;
end;
figure;
[no,bins] = hist(z,10);
clear z;
semilogy(bins,no/nsamples,'b*');
title('Quat2RotVec(RotVec2Quat(.)) Error Histogram');
xlabel('|\rho_{Output}-\rho_{Input}|/(2*Pi)');
ylabel(['Rel. No. of Occurrences in ',num2str(nsamples),' Random Samples']);

