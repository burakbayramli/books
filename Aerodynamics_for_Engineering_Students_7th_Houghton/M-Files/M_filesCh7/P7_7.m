% P5_7: G = gam/gam0
clear;clc
% y = -1:.1:1;
th = 0:.01:pi;
GG = (1 - cos(th).^2).^(1/2);%.^(3/2);
yy = cos(th);
ainF = 5.8; AR = 7;
mu0 = ainF/pi/AR; % This is correct because s in 6e is 2s in 5e. 
% C = 3*gam0/16/s/V;
C = (2*pi/180)*mu0/( sin(pi/2) ) ;% - sin(3*pi/2)/3 + mu0*(1 - sin(3*pi/2)/sin(pi/2)));
mu = mu0.*sin(th);
alpha = (C./mu).*( sin(th) );%- sin(3.*th)/3 + mu.*(1 - sin(3.*th)./sin(th)));
plot(yy,180*alpha/pi)
xlabel('spanwise coordinate, y')
ylabel('Geometric angle of attack, \alpha')
title('Distribution of geometric angle of attack')
disp(['Angle at each tip'])
disp([alpha(2)*180/pi,alpha(end-1)*180/pi])
% Downwash
figure(2)
w =  C *(1 );%* - sin(3*th)./sin(th));
plot(yy,w)
xlabel('spanwise coordinate, y'),ylabel('downwash, w')
title('downwash distribution')
figure(3)
plot(yy,GG)
xlabel('spanwise coordinate, y'),ylabel('Circulation, G')
title('Circulation distribution')