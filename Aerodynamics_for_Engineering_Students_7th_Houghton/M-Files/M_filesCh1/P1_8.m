% Problem 1.8
clear;clc
dtheta = 10;
theta = 0:dtheta:180; % degrees
thetac = theta + dtheta/2;
p = [569 502 301 -57 -392 -597 -721 -726 -707 -660 -626 ...
    -588 -569 -569 -569 -569 -569 -569 -569]; % N/m/m/
%plot(theta,p)
D = 150/1000; % m
V = 30; % m/s
rho = 1.23; % kg/m/m/m
R = D/2;
Dr = 0;
for n = 1:length(theta)-1;
    dAx(n) = R * (dtheta*pi/180) * cos( thetac(n)*pi/180 ); 
    dN(n) = dAx(n) * ( p(n) + p(n+1) ) / 2;
    Dr = Dr + dN(n);
end
Dr = 2*Dr
CD = Dr/(rho*V^2*D/2)
plot(thetac(1:end-1),dAx)
figure(2)
plot(thetac(1:end-1),dN)

