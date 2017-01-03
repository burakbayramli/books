function Quat = RotVec2Quat(RotVec);
%
% Converts rotation vector to quaternion
% 
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
%
% 1. Condition rotation vector modulo 2*Pi to be of length <= 2*pi
%
thetaRaw    = sqrt(RotVec(1)^2+RotVec(2)^2+RotVec(3)^2);
thetaMod2Pi = mod(thetaRaw,2*pi);
if thetaMod2Pi>pi 
    thetaMod2Pi=thetaMod2Pi-2*pi; 
end;
%
% 2. Condition rotation vector to have nonnegative rotation angle
%    0 <= theta <= pi, and compute associated unit vector UnitVec.
%
if thetaMod2Pi == 0
    signum = 1;
else
    signum = sign(thetaMod2Pi);
end;
UnitVec     = (signum/thetaRaw)*RotVec;
theta       = abs(thetaMod2Pi);
%
% 3. Compute cos and sin of half-theta
%
CosHalfTheta    = cos(theta/2);
SinHalfTheta    = sin(theta/2);
%
% 4. Calculate quaternion as column 4-vector
%
Quat            = [CosHalfTheta;SinHalfTheta*UnitVec];
return;