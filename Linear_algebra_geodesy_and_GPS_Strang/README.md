

http://gpsworld.com/innovation-precise-positioning-using-raw-gps-measurements-from-android-smartphones/

how to convert gnss api data to pseudorange:

From http://garrett.seepersad.org/blog/where-are-my-gnss-measurements

%% Constants
C                     = 299792458; % Speed of light
weekSeconds           = 604800;    % Seconds in a week
 
%% Input
getTimeOffsetSeconds  = 0; % getTimeOffsetNanos*1E-9
getBiasSeconds        = 0; % getBiasNanos*1E-9
FullBiasSeconds       = -1155937562.91587; % getFullBiasNanos() *1E-9
TimeSeconds           = 17.084;            % getTimeNanos() *1E-9
ReceivedSvTimeSeconds = 164779.928555738;  % getReceivedSvTimeNanos() *1E-9
 
%% Calculate pseudorange
weekNumber            = floor(-1*(FullBiasSeconds)/weekSeconds); % Calculate GPS week
weekNumberSeconds     = (weekNumber)*(weekSeconds); 
secondsOfWeek         = (-1*FullBiasSeconds) - weekNumberSeconds - getBiasSeconds; % Seconds of the week the device was turned on
TimeTransmitted       = (ReceivedSvTimeSeconds);
TimeReceived          = (secondsOfWeek + TimeSeconds + getTimeOffsetSeconds);
 
C1Pseudorange         = (TimeReceived-TimeTransmitted)*C
