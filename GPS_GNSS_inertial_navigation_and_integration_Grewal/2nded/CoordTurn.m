function [lat,lon,alt,roll,pitch,heading,rollrate,pitchrate,yawrate,accroll,accpitch,accyaw,Time] = CoordTurn(lat0,lon0,alt0,heading0,newheading,pitch0,Vel0,t0)
%
% Approximates navigation conditions for a passenger jet during a
% coordinated turn of 1-3 minutes duration (depending on heading change).
% Conditions are sampled at 1-second intervals and output as row vectors.
% Pitch angle and speed are held constant during the turn.  The program
% uses table-lookup from simulations of a smooth roll manuever: 
%          [roll angle = MRA * (1-cos(pi*t/Dt)) ]
% to calculate manuever time Dt and maximum roll angle MRA as function of
% aircraft speed and required heading change.  Navigation calculations use
% a "flat earth" approximation during the turn.  Conversion to latitude and
% longitude assumes a spherical earth model with earth radius = 6371200 m.
% Angular outputs include only rotations with respect to the earth: no
% earth rotation rates are included.
%
% INPUTS
%   lat0        = initial latitude [deg]
%   lon0        = initial longitude, +east from Greenwich [deg]
%   alt0        = initial altitude above sea level [m]
%   heading0    = current heading, clockwise from north [deg]
%   newheading  = desired heading, clockwise from north [deg]
%   pitch0      = pitch angle (assumed constant through turn) [deg]
%   Vel0        = velocity (assumed constant magnitude through turn) [m/s]
%   t0          = starting time [sec]
%
% OUTPUTS: values sampled at 1-second intervals, starting 1 second after t0
%   lat         = latitude [deg]
%   lon         = longitude, east from Greenwich [deg]
%   alt         = altitude above sea level [m]
%   roll        = vehicle roll angle [deg]
%   pitch       = vehicle pitch angle [deg]
%   heading     = vehicle heading, measured clockwise from north [deg]
%   rollrate    = intertial rate about vehicle roll axis [deg/sec]
%   pitchrate   = intertial rate about vehicle pitch axis [deg/sec]
%   yawrate     = intertial rate about vehicle yaw axis [deg/sec]
%   accroll     = accelerometer-sensed acceleration along roll axis
%   accpitch    = accelerometer-sensed acceleration along pitch axis
%   accyaw      = accelerometer-sensed acceleration along yaw axis
%   Time        = time
%
% Table of simulated heading changes [deg] for 60-second turns
%
DeltaHeadDeg60 = [0;1.2100;2.4204;3.6317;4.8444;6.0590;7.2759;8.4956;
    9.7186;10.9453;12.1763;13.4120;14.6531;15.8999;17.1530;18.4130;19.6805;
    20.9559;22.2400;23.5333;24.8364;26.1500;27.4749;28.8116;30.1609;
    31.5236;32.9005;34.2925;35.7003;37.1248;38.5672;40.0283;41.5092;
    43.0110;44.5349;46.0821;47.6540;49.2519;50.8773;52.5319;54.2172;
    55.9350;57.6874;59.4762;61.3038;63.1724;65.0846;67.0431;69.0510;
    71.1113;73.2276;75.4037;77.6436;79.9518;82.3333;84.7935;87.3384;
    89.9744;92.7089;95.5500;98.5066];
%
% Table of simulated maximum roll angles [deg]
%
MaxRollDeg60    = (0:60)';
%
G               = 9.8; % acceleration due to gravity [m/s/s]
NominalVel      = 243; % velocity used for look-up table values
%
% Transform heading change to be between -180 and +180 deg
%
DeltaHeading    = newheading - heading0;
if DeltaHeading > 180
    DeltaHeading    = DeltaHeading - 360;
elseif DeltaHeading < -180
    DeltaHeading    = DeltaHeading + 360;
end;
%
% If heading change is beyond 60 degrees, scale up the manuever time
% by a factor of 3 and scale down the lookup value by a factor of 3.
% This keeps the commanded roll angle from becoming more than 45 degrees.
%
if abs(DeltaHeading) <= 60;
    AbsMaxRollDeg   = interp1(DeltaHeadDeg60,MaxRollDeg60,abs(DeltaHeading));
    ManueverTime    = 60;
else
    AbsMaxRollDeg   = interp1(DeltaHeadDeg60,MaxRollDeg60,abs(DeltaHeading)/3);
    ManueverTime    = 180;
end;
%
% Compensate for the fact that tabulated values are all + heading changes
%
MaxRollDeg  = sign(DeltaHeading)*AbsMaxRollDeg*Vel0/NominalVel;
%
% Initialize the 1-sample-per-second loop
%
k           = 0;
dt          = .001; % navigation integration time-step
Northing    = 0; % locally level coordinates at starting position
Easting     = 0; % for integrating position & velocity
DeltaAlt    = 0; % altitude change
HeadRad     = heading0*pi/180;  % heading in radians
PitchRad    = pitch0*pi/180;    % pitch  in radians
cp0         = cos(PitchRad);    % cosine of pitch angle
sp0         = sin(PitchRad);    % sine of pitch angle
clat        = cos(lat0*pi/180); % cosine of latitude
%
% Outer loop is at one-second sample times
%
for T=0:ManueverTime-1,
    %
    % Inner navigation integration loop uses millisecond time intervals
    % and rectangular integration  from time t = T to t = T + 1.
    %
    for t=T:dt:T+1;
        RollRad         = MaxRollDeg*pi*(1 - cos(2*pi*t/ManueverTime))/360;
        TanRoll         = tan(RollRad);
        ALat            = G*TanRoll; % lateral acceleration
        DeltaVLat       = ALat*dt;   % change in lateral velocity
        DeltaHeadRad    = DeltaVLat/Vel0/cp0; % heading change in time dt
        HeadRad         = HeadRad + DeltaHeadRad; % heading in radians
        cHR             = cos(HeadRad);
        sHR             = sin(HeadRad);
        Northing        = Northing + (cHR*Vel0*cp0 - sHR*DeltaVLat)*dt;  
        Easting         = Easting + (sHR*Vel0*cp0 + cHR*DeltaVLat)*dt; 
        DeltaAlt        = DeltaAlt + Vel0*sp0*dt; % altitude change
    end;
    k               = k + 1; % sample index
    alt(k)          = alt0 + DeltaAlt;
	lat(k)          = lat0 + Northing/(6371200+alt(k))*180/pi;
    lon(k)          = lon0 + Easting/clat/(6371200+alt(k))*180/pi;
    if lon(k) > 180
        lon(k) = lon(k) - 360;
    elseif lon(k) < -180
        lon(k) = lon(k) + 360;
    end;
    roll(k)         = RollRad*180/pi;
    sRR             = sin(RollRad);
    cRR             = cos(RollRad);
    pitch(k)        = pitch0;
    heading(k)      = HeadRad*180/pi;
    HeadingRate     = 180*DeltaHeadRad/dt/pi; % rate about downward vert.
    rollrate(k)     = MaxRollDeg*pi*sin(2*pi*(T+1)/ManueverTime)/ManueverTime - HeadingRate*sp0;
    pitchrate(k)    = HeadingRate*sRR*cp0;
    yawrate(k)      = HeadingRate*cRR*cp0;
    accroll(k)      = G*sp0;  % roll component of thrust minus drag
    accpitch(k)     = 0;      % zero thrust, drag and lift
    accyaw(k)       = -G/cRR; % lift augmented to compensate for roll
    Time(k)         = t0 + T + 1;
end;

    