
dateStructure.year = 2016;
dateStructure.month = 12;
dateStructure.day = 10;
dateStructure.hour = 1;
dateStructure.min = 1;
dateStructure.sec = 1;

timeVector = [233472]

almanac.prn = 1
almanac.e = 0.6062030792E-002
almanac.toa = 233472.0000
almanac.i = 0.9661825929
almanac.OMEGAo = 0.4904225712E+000
almanac.OMEGAdot = -0.7611745631E-008
almanac.a = 5153.650879
almanac.omega = 0.559771536
almanac.M = -0.9205300364E+000

[posECEF, posVelECI] = getSatelliteState2(almanac, dateStructure, timeVector)
