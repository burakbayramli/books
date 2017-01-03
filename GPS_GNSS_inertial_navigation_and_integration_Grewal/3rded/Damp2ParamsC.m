function [TauVel,RhoVelAcc,SigmaJerk] = DAMP2ParamsC(SigmaAcc,SigmaVel,TauAcc,DeltaT);
%
% Computes attributes of the DAMP2 stochastic model in continuous time
%
% INPUTS
%   SigmaAcc  = RMS acceleration
%   SigmaVel  = RMS velocity
%   TauAcc    = correlation time-constant of accelerations
%   DeltaT    = discrete time interval
%
% OUTPUTS      = Parameters of continuous-time model
%   TauVel     = Correlation time of velocity
%   RhoVelAcc  = Steady-state correlation coefficient between Vel & Acc
%   SigmaJerk  = RMS jerk noise
TauVel      = SigmaVel*(SigmaVel + sqrt(SigmaVel^2 + 4*TauAcc^2*SigmaAcc^2))/SigmaAcc^2/TauAcc/2;
RhoVelAcc   = SigmaVel/SigmaAcc/TauVel;
if abs(RhoVelAcc) >=1, error('DAMP3Params: Incompatible inputs, |RhoVelAcc| >= 1'); end;
F           = [-1/TauVel, 1; 0, -1/TauAcc];
Phi         = expm(DeltaT*F);
P           = [SigmaVel^2, RhoVelAcc*SigmaVel*SigmaAcc; RhoVelAcc*SigmaVel*SigmaAcc,SigmaAcc^2];
Q           = P - Phi*P*Phi';
SigmaJerk   = SigmaAcc*sqrt(1-exp(-2*DeltaT/TauAcc))/DeltaT^2;
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  

