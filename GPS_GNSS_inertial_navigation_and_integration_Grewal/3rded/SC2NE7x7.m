function FSC2NE7x7 = SC2NE7x7(aENU)
%
% Calculates dynamic coefficient sub-matrix coupling 7 sensor compensation
% errors into 7 (horizontal-only) navigation errors.
%
% Seven sensor compensation parameter errors:
%
%   1. East accelerometer bias
%   2. North accelerometer bias
%   3. East accelerometer scale factor
%   4. North accelerometer scale factor
%   5. East gyro bias
%   6. North gyro bias
%   7. Up gyro bias 
%
% Seven navigation errors:
%
%   1. East location error
%   2. North location error
%   3. East velocity error
%   4. North velocity error
%   5. East misalignment error
%   6. North misalignment error
%   7. Up (heading) misalignment error 
%
FSC2NE7x7 = [zeros(2,7);eye(2),[aENU(1),0;0,aENU(2)],zeros(2,3);zeros(3,4),eye(3)];
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
