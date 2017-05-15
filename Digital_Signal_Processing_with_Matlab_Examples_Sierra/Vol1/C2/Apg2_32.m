% Curve and area
%the curve
x=0:0.1:10;
y=0.5+(0.3*sin(0.8*x));

plot(x,y,'k'); hold on;
axis([0 10 0 1]);
for vx=1:1:10,
   nx=vx*10;
   plot([vx vx],[0 y(nx+1)],'g','linewidth',2);
end;
xlabel('x'); ylabel('y');
title('area covered by a curve');
