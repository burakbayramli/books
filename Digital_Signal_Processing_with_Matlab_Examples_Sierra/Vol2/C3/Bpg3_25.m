% Color wheel (hue)

N=32; %number of segments
R=(60:10:100)'; %radius zone
phi=2*pi*(0:N)/N;
zz=ones(length(R),1);

x=R*cos(phi);
y=R*sin(phi);
c=zz*(phi/(2*pi));

figure(1)
colormap('HSV');
pcolor(x,y,c);
shading interp;
axis equal;
title('color wheel: Hue=0, red; Hue=0.5, cyan');



