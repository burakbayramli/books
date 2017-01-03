function RotVec = Quat2RotVec(Quat)
%
% Converts quaternion to equivalent rotation vector
% 
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
%
CosHalfTheta = Quat(1);
SinHalfTheta = sqrt(Quat(2)^2+Quat(3)^2+Quat(4)^2);
HalfTheta    = atan2(SinHalfTheta,CosHalfTheta);
%
Theta        = 2*HalfTheta;
if Theta == 0
    UnitVec = zeros(3,1);
else
    UnitVec = Quat(2:4)/SinHalfTheta;
    UnitVec = (1/norm(UnitVec))*UnitVec;
end;
RotVec = Theta*UnitVec;
return;