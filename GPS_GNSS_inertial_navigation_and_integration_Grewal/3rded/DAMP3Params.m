function [Phi,Q,P] = DAMP3Params(SigmaAcc,SigmaVel,SigmaPos,TauAcc,DeltaT);
%
% Computes parameters of the DAMP3 stochastic model, given desired values:
%
%   SigmaAcc  = RMS acceleration
%   SigmaVel  = RMS velocity
%   SigmaPos  = RMS position variation
%   TauAcc    = correlation time-constant of accelerations
%   DeltaT    = discrete time interval
%
% OUTPUTS   = Parameters of discrete-time model
%   Phi     = State transition matrix
%   Q       = Process noise covariance
%   P       = Steady-state covaraince matrix of state uncertainty
P(1,1)      = SigmaPos^2;
P(2,2)      = SigmaVel^2;
P(3,3)      = SigmaAcc^2;
TauVel      = (P(2,2) + sqrt(P(2,2)^2 + 4*P(3,3)*TauAcc^2*P(2,2)))/2/P(3,3)/TauAcc;
P(2,3)      = P(2,2)/TauVel;
% [p11 * tau(vel) * tau(acc) p11 * (tau(acc) + tau(vel)) -p22 * tau(vel) * tau(acc) + p11 -tau(vel) * (p23 * tau(acc) + p22)];
c3          = -TauVel*(P(2,3)*TauAcc + P(2,2));
c2          = -P(2,2)*TauVel*TauAcc + P(1,1);
c1          = P(1,1)*(TauAcc + TauVel);
c0          = P(1,1)*TauVel*TauAcc;
TauPosPoly  = [c3,c2,c1,c0];
TauPosRoots = roots(TauPosPoly);
TauPos      = max(TauPosRoots);
P(1,2)      = P(1,1)/TauPos;
P(1,3)      = P(2,3)*TauPos*TauAcc/(TauAcc + TauPos);
P(2,1)      = P(1,2);
P(3,1)      = P(1,3);
P(3,2)      = P(2,3);
Peigs       = eig(P);
if min(Peigs)<=0 error('DAM3Params failed: nonpositive definite solution for P');end;
F           = [-1/TauPos, 1, 0; 0, -1/TauVel, 1; 0, 0, -1/TauAcc];
Phi         = expm(DeltaT*F);
Q           = P - Phi*P*Phi';
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
