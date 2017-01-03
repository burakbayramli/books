%*******************************************************
% function [los_NED] = ecef2ned(Ref_LLA,los)
%
% DESCRIPTION:
%  Transforms line-of-sight vector from ECEF coordinate frame
%  to NED (North-East_Down) frame.
%  
% ARGUMENTS:
%  Ref_LLA - Reference position in lat, lon, alt (deg, m)
%  los - Line of sight vector to convert
%  
% OUTPUT:
%  los_NED - Vector in NED frame
%  
% CALLED BY:
%  getAzEl
%
% FUNCTIONS CALLED:
%  None
%*******************************************************
function [los_NED] = ecef2ned(Ref_LLA,los);

clat = cos(Ref_LLA(1)*pi/180);
slat = sin(Ref_LLA(1)*pi/180);
clon = cos(Ref_LLA(2)*pi/180);
slon = sin(Ref_LLA(2)*pi/180);

% Transformation matrix for ECEF to NED
C = [-clon*slat, -slon*slat, clat; -slon, clon, 0; -clon*clat, -slon*clat, -slat];

los_NED = C*los;