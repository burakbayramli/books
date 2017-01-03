function QuatNormalized = QuatNormalize(Quat)
%
% Normalizes quaternion to have magnitude = 1
% 
% M. S. Grewal, L. R. Weill and A. P. Andrews
% Global Positioning Systems, Inertial Navigation, and Integration
% 2nd Edition, Wiley, 2006
%
QuatNormalized = (1/norm(Quat))*Quat;
return;
