function [Phi,Q,P] = DAMP2Params(SigmaAcc,SigmaVel,TauAcc,DeltaT);
%
% Computes parameters of the DAMP2 stochastic model, given desired values:
%
% INPUTS
%   SigmaAcc  = RMS acceleration
%   SigmaVel  = RMS velocity
%   TauAcc    = correlation time-constant of accelerations
%   DeltaT    = discrete time interval
%
% OUTPUTS   = Parameters of discrete-time model
%   Phi     = State transition matrix
%   Q       = Process noise covariance
%   P       = Steady-state covaraince matrix of state uncertainty
TauVel      = SigmaVel*(SigmaVel + sqrt(SigmaVel^2 + 4*TauAcc^2*SigmaAcc^2))/SigmaAcc^2/TauAcc/2;
RhoVelAcc   = SigmaVel/SigmaAcc/TauVel;
if abs(RhoVelAcc) >=1, error('DAMP3Params: Incompatible inputs, |RhoVelAcc| >= 1'); end;
F           = [-1/TauVel, 1; 0, -1/TauAcc];
Phi         = expm(DeltaT*F);
P           = [SigmaVel^2, RhoVelAcc*SigmaVel*SigmaAcc; RhoVelAcc*SigmaVel*SigmaAcc,SigmaAcc^2];
Q           = P - Phi*P*Phi';
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
