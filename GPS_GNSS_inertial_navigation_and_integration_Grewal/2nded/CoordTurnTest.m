%
% Check out the coordinated turn simulator CoordTurn.m with two cases
%
% 1. Turn from LAX runway 25L heading (263 deg true heading) to -5 deg
%    at cruise speed (243 m/s) and altitude (10 km).
% 2. Same turn at lower speed (200 m/s), same altitude, with 11 deg pitch.
%
clear all;
close all;
lat = 33.9425361;
lon = -118.4080744;
%
lat0 = 33.9425361;
lon0 = -118.4080744;
alt0 = 10000;
heading0 = 263-360;
newheading = -5;
pitch0 = 0;
Vel0 = 243;
t0 = 0;
pitch1 = 11;
Vel1 = 200;
[lat,lon,alt,roll,pitch,heading,rollrate,pitchrate,yawrate,accroll,accpitch,accyaw,Time] = CoordTurn(lat0,lon0,alt0,heading0,newheading,pitch0,Vel0,t0);
[lat1,lon1,alt1,roll1,pitch1,heading1,rollrate1,pitchrate1,yawrate1,accroll1,accpitch1,accyaw1,Time1] = CoordTurn(lat0,lon0,alt0,heading0,newheading,pitch1,Vel1,t0);
figure;
plot(lon,lat,'k-',lon1,lat1,'k:');
legend('0^o pitch, 243 m/s','11^o pitch, 200 m/s');
xlabel('Longitude [deg]');
ylabel('Latitude [deg]');
title('Turn to -5 deg heading from 263 deg heading');
axis equal;
%
figure;
subplot(3,1,1),
plot(Time,roll,'k-',Time,roll1,'k:');
%legend('0^o pitch','11^o pitch');
title('Turn to -5 deg heading from 263 deg heading');
ylabel('Roll Angle [deg]');
subplot(3,1,2),
plot(Time,pitch,'k-',Time,pitch1,'k:');
ylabel('Pitch Angle [deg]');
subplot(3,1,3),
plot(Time,heading,'k-',Time,heading1,'k:');
ylabel('Heading Angle [deg]');
xlabel('Time [sec]');
%
figure;
subplot(3,1,1),
plot(Time,rollrate,'k-',Time,rollrate1,'k:');
%legend('0^o pitch','11^o pitch');
title('Turn to -5 deg heading from 263 deg heading');
ylabel('Omega Roll [deg/s]');
subplot(3,1,2),
plot(Time,pitchrate,'k-',Time,pitchrate1,'k:');
ylabel('Omega Pitch [deg/s]');
subplot(3,1,3),
plot(Time,yawrate,'k-',Time,yawrate1,'k:');
ylabel('Omega Yaw [deg/s]');
xlabel('Time [sec]');
%
figure;
subplot(3,1,1),
plot(Time,accroll,'k-',Time,accroll1,'k:');
%legend('0^o pitch','11^o pitch');
title('Turn to -5 deg heading from 263 deg heading');
ylabel('Roll Accel. [m/s/s]');
subplot(3,1,2),
plot(Time,accpitch,'k-',Time,accpitch1,'k:');
ylabel('Pitch Accel. [m/s/s]');
subplot(3,1,3),
plot(Time,accyaw,'k-',Time,accyaw1,'k:');
ylabel('Yaw Accel. [m/s/s]');
xlabel('Time [sec]');
%
figure;
plot(Time,alt,'k-',Time,alt1,'k:');
title('Turn to -5 deg heading from 263 deg heading');
legend('0^o pitch','11^o pitch');
ylabel('Altitude [m]');
xlabel('Time [sec]');
