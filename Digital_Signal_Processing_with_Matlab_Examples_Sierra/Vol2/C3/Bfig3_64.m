% Line % perpendicular for one point

%the point
x0=5; y0=4;

tg=y0/x0; %tangent
beta=atan(tg); %radius angle
tg2=tan((pi/2)-beta);

dx=y0/tg2; dy=x0/tg;

X=x0+dx; 
Y=y0+dy;

figure(1)
%axes
plot([0 12],[0 0],'b'); hold on;
plot([0 0],[0 12],'b');
%lines
plot([0 x0],[0 y0],'r'); %radius
plot([0 X],[Y 0],'k'); %line
%the point
plot(x0,y0,'bd');
title('line and perpendicular crossing (x0,y0)');
   