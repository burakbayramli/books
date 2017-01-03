function draw_MBK(n,w,y,u,color)
%n: the # of spring windings
%w: the width of each object
%y: displacement of the top of MBK
%u: displacement of the bottom of MBK
if nargin<5, color='k'; end
p1=[-w  u+4];  p2=[-w  9+y];
xm=0; ym=(p1(2)+p2(2))/2; 
%Mass
xM= xm+w*1.2*[-1 -1  1  1 -1];
yM= p2(2)+w*[1 3  3  1  1];
plot(xM,yM,color)
hold on
%Spring
spring(n,p1,p2,w,color)
%Damper
damper(xm+w,p1(2),p2(2),w,color)
%Wheel
wheel_my(xm,p1(2)-3*w,w,color)