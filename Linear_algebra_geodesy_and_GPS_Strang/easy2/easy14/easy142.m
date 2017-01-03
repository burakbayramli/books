% Plot of elevation angle for a geostationary satellite
% as seen from a position on the surface on the Earth
% at latitude y = phi

% Written July 31, 2008
% by Kai Borre

set(0,'DefaultTextFontName','Times');
set(0,'DefaultAxesFontName','Times');
set(0,'DefaultTextFontSize',16);

%aG/aE = 6.34;
aE = 6700;
aG = 42486;
x = 0:pi/50:pi/2;
y = atan( (cos(x)-aE/aG)./sin(x));

figure(1);
plot(x*180/pi,y*180/pi,'-r','linewidth',2)
xlabel('Latitude {\phi}','fontsize',16,'fontname','Times')
ylabel('Elevation Angle {\ith}','fontsize',16,'fontname','Times')
set(gca,'Fontsize',16);
print -depsc2 easy1422

% Sample of computing elevation angle y = h for given x = phi
x = 57*pi/180;
y = atan(abs(aE/aG-cos(x))/sin(x));
y = y*180/pi 

%%%%%%%%%%%%% end easy142.m  %%%%%%%%%%%%%%%%%%%%