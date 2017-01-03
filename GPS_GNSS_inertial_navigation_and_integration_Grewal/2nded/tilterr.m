%
% Compute and plot peak accumulated position error in as a function of
% tilt error, including Schuler effect
%
clear all;
close all;
log10tilt = -8:.01:0;
tilt      = 10.^log10tilt;
sintilt   = sin(tilt);
g         = 9.8; % gravitational acceleration [m/s^2]
t         = 42*60; % seconds in 42 minutes (1/2 Schuler period)
L         = 6.3056e+006; % arm length of Schuler pendulum [m]
for k=1:length(tilt);
    a = sintilt(k)*g;
    v(k) = a*t;
    x(k) = 1/2*a*t^2; % flat earth model after 1/2 Schuler period
    s(k) = 2*L*sintilt(k);
end;
loglog(tilt,s,'k-');
xlabel('Tilt Error [rad]');
ylabel('Peak Position Error [m]');
title('Effect of Attitude Tilt Error on Position Error');
    
    
