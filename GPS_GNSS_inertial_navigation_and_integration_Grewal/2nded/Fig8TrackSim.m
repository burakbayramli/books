function VehState = Fig8TrackSim(Time,TrackLength,Speed,CrossOverHeight)
%
% Simulator for vehicle on a figure-eight track, in locally level
% coordinates, with near-critical banking
%
% INPUTS
%   Time            time in seconds from crossover point
%   TrackLength     track length in meters
%   Speed           average vehicle speed in meters per second
%   CrossOverHeight height of bridge at crossover
%
% OUTPUTS
%   VehState        vector of vehicle state variables:
%       PosN        Northing from crossover [meters]
%       PosE        Easting from crossover [meters]
%       PosD        Downward position wrt median crossover [meters]
%       VelN        North velocity [m/s]
%       VelE        East velocity [m/s]
%       VelD        Downward velocity [m/s]
%       AccN        North acceleration [m/s/s]
%       AccE        East acceleration [m/s/s]
%       AccD        Downward acceleration [m/s/s] (not including gravity)
%       Roll        Vehicle roll angle [rad]
%       Pitch       Vehicle pitch angle, up from horizontal [rad]
%       Heading     Vehicle heading measured clockwise from north [rad]
%       RollRate    Vehicle rotation rate about its roll axis [rad/s]
%       PitchRate   Vehicle rotation rate about its pitch axis [rad/s]
%       YawRate     Vehicle rotation rate about its yaw axis [rad/s]
%       AccR        Acceleration along vehicle roll axis [m/s/s]
%       AccP        Acceleration along vehicle pitch axis [m/s/s]
%       AccY        Acceleration along vehicle yaw axis [m/s/s]
%                   (not including gravity)
%       AccR        Vehicle acceleration along its roll axis [m/s/s];
%       AccP        Vehicle acceleration along its pitch axis [m/s/s];
%       AccY        Vehicle acceleration along its yaw axis [m/s/s];
%
S         = TrackLength/14.94375529901562;  % track scaling parameter
h         = -CrossOverHeight;
omega     = 2*pi*Speed/TrackLength;
G         = 9.8;
MaxRoll   = asin(3*omega^2*S/G);
theta     = omega*Time;
s1        = sin(theta);
c1        = cos(theta);
s2        = sin(2*theta);
c2        = cos(2*theta);
% Northing from crossover [meters]
PosN      = 3*S*s1;
% Easting from crossover [meters]
PosE      = S*s2;
% Downward position wrt median crossover [meters]
PosD      = -h*c1/2;
% North velocity [m/s]
VelN      = 3*S*c1*omega;
% East velocity [m/s]
VelE      = 2*S*c2*omega;
% Downward velocity [m/s]
VelD      = h*s1*omega/2;
% North acceleration [m/s/s]
AccN      = -3*S*s1*omega^2;
% East acceleration [m/s/s]
AccE      = -4*S*s2*omega^2;
% Downward acceleration [m/s/s] (not including gravity)
AccD      = h*c1*omega^2/2 - G;
% Vehicle roll angle [rad]
Roll      = MaxRoll*s1;
% Vehicle pitch angle, up from horizontal [rad]
Pitch     = atan2(h*s1/2,S*(9*c1^2 + 4*c2^2)^(1/2));
% Vehicle heading measured clockwise from north [rad]
Heading   = atan2(2*c2,3*c1);
% Coordinate transformation from North-East-Down to Roll-Pitch-Yaw
sR = sin(Roll);
cR = cos(Roll);
sP = sin(Pitch);
cP = cos(Pitch);
sH = sin(Heading);
cH = cos(Heading);
CR = [ 1 , 0 , 0 ; 0 , cR , sR ; 0 , -sR , cR ] ;
CP = [ cP , 0 , -sP ; 0 , 1 , 0 ; sP , 0 , cP ] ;
CH = [ cH , sH , 0 ; -sH , cH , 0 ; 0 , 0 , 1 ] ;
CNED2RPY = CR * CP * CH;% Rate of vehicle rotation about its roll axis [rad/s]
RateRoll  = MaxRoll*c1*omega;
% Rate of increase in pitch angle [rad/s]
RatePitch = 2*h*omega*S*(4*c1*c2^2 + 9*c1 + 8*s1*c2*s2)/(9*c1^2 + 4*c2^2)^(1/2)/(36*S^2*c1^2 + 16*S^2*c2^2 + h^2*(1 - c1^2));
% Rate of change in vehicle heading [rad/s]
RateAz    = 6*omega*(s1*c2 - 2*c1*s2)/(9*c1^2 + 4*c2^2);
% Rotation rate in NED coordinates due to RatePitch
OmegaPitch = (RatePitch/sqrt(1 - (cP*sR)))*[-sH*cR+cH*sP*sR ; cH*cR+sH*sP*sR ; 0];
% Rotation rate in NED coordinates dur to RateAz
OmegaAz   = [0 ; 0 ; RateAz ];
% Rotation rate in Roll-Pitch-Yaw coordinates
OmegaRPY  = [RateRoll;0;0] + CNED2RPY*(OmegaAz + OmegaPitch);
RollRate  = OmegaRPY(1);
PitchRate = OmegaRPY(2);
YawRate   = OmegaRPY(3);
AccRPY    = CNED2RPY*[AccN;AccE;AccD];
AccR      = AccRPY(1);
AccP      = AccRPY(2);
AccY      = AccRPY(3);
VehState  = [PosN;PosE;PosD;VelN;VelE;VelD;AccN;AccE;AccD;Roll;Pitch;Heading;RollRate;PitchRate;YawRate;AccR;AccP;AccY];
% where
% PosN      = Northing from crossover [meters]
% PosE      = Easting from crossover [meters]
% PosD      = Downward position wrt median crossover [meters]
% VelN      = North velocity [m/s]
% VelE      = East velocity [m/s]
% VelD      = Downward velocity [m/s]
% AccN      = North acceleration [m/s/s]
% AccE      = East acceleration [m/s/s]
% AccD      = Downward acceleration [m/s/s] (not including gravity)
% Roll      = Vehicle roll angle [rad]
% Pitch     = Vehicle pitch angle, up from horizontal [rad]
% Heading   = Vehicle heading measured clockwise from north [rad]
% RollRate  = Vehicle rotation rate about its roll axis [rad/s]
% PitchRate = Vehicle rotation rate about its pitch axis [rad/s]
% YawRate   = Vehicle rotation rate about its yaw axis [rad/s]
% AccR      = Vehicle acceleration along its roll axis [m/s/s];
% AccP      = Vehicle acceleration along its pitch axis [m/s/s];
% AccY      = Vehicle acceleration along its yaw axis [m/s/s];

