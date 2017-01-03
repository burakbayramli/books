clear all;
close all;
SigmaAcc    = .05*9.8;
SigmaVel    = 1;
SigmaPos    = 10;
TauAcc      = 1;
DeltaT      = 1;
%
% Computes parameters of the DAMP3 stochastic model, given desired values:
%
%   SigmaAcc  = RMS acceleration
%   SigmaVel  = RMS velocity
%   SigmaPos  = RMS position variation
%   TauAcc    = correlation time-constant of accelerations
%   DeltaT    = discrete time interval
%
TauVel      = SigmaVel*(SigmaVel + sqrt(SigmaVel^2 + 4*SigmaAcc^2*TauAcc^2))/SigmaAcc^2/TauAcc/2;
RhoVelAcc   = 2*SigmaAcc*TauAcc/sqrt(SigmaVel^2 + 4*SigmaAcc^2*TauAcc^2);
if abs(RhoVelAcc) >=1, error('DAMP3Params: Incompatible inputs, |RhoVelAcc| >= 1'); end;
c3          = -TauVel*SigmaVel*(SigmaVel + SigmaAcc*RhoVelAcc*TauAcc);
c2          = -SigmaVel^2*TauVel*TauAcc + SigmaPos^2;
c1          = SigmaPos^2*(TauAcc + TauVel);
c0          = SigmaPos^2*TauVel*TauAcc;
TauPosPoly  = [c3,c2,c1,c0];
TauPosRoots = roots(TauPosPoly);
TauPos      = max(TauPosRoots);
if TauPos <= 0, error('DAMP3Params: Incompatible inputs, TauPos <= 0'); end;
RhoPosVel   = SigmaPos/SigmaVel/TauPos;
if abs(RhoPosVel) >=1, error('DAMP3Params: Incompatible inputs, |RhoPosVel| >= 1'); end;
RhoPosAcc   = RhoVelAcc*SigmaVel*TauPos*TauAcc/SigmaPos/(TauAcc + TauPos);
if abs(RhoPosAcc) >=1, error('DAMP3Params: Incompatible inputs, |RhoPosAcc| >= 1'); end;
F           = [-1/TauPos, 1, 0; 0, -1/TauVel, 1; 0, 0, -1/TauAcc];
Phi         = expm(DeltaT*F);
P           = [SigmaPos^2,RhoPosVel*SigmaPos*SigmaVel,RhoPosAcc*SigmaPos*SigmaAcc;
    RhoPosVel*SigmaPos*SigmaVel,SigmaVel^2,RhoVelAcc*SigmaVel*SigmaAcc;
    RhoPosAcc*SigmaPos*SigmaAcc,RhoVelAcc*SigmaVel*SigmaAcc,SigmaAcc^2];
Q           = P - Phi*P*Phi';