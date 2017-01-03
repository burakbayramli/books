function FSC2NE9x9 = SC2NE9x9(aSensedENU)
%
% Calculates dynamic coefficient sub-matrix coupling 9 sensor compensation
% errors into 9 (horizontal-only) navigation errors.
%
% Nine sensor compensation parameter errors:
%
%   1. East accelerometer bias
%   2. North accelerometer bias
%   3. Up accelerometer bias
%   4. East accelerometer scale factor
%   5. North accelerometer scale factor
%   6. North accelerometer scale factor
%   7. East gyro bias
%   8. North gyro bias
%   9. Up gyro bias 
%
% Nine navigation errors:
%
%   1. East location error
%   2. North location error
%   3. Up location error (altitude)
%   4. East velocity error
%   5. North velocity error
%   6. Uo velocity error
%   7. East misalignment error
%   8. North misalignment error
%   9. Up (heading) misalignment error 
%
FSC2NE9x9 = [zeros(3,9)
    eye(3),[aSensedENU(1),0,0;0,aSensedENU(2),0;0,0,aSensedENU(3)],zeros(3,3)
    zeros(3,6),eye(3)];
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
