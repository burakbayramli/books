function QuatNormalized = QuatNormalize(Quat)
%
% Normalizes quaternion to have magnitude = 1
% 
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
%
QuatNormalized = (1/norm(Quat))*Quat;
return;
