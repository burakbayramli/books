% Zeichnet geschlossene Spline-Kurve
clf
axis([0 10 0 10]);
hold on
% initially, the list of points is empty
xy = [];
n = 0;
% Loop, picking up the points
disp('left mouse button picks points.')
disp('right mouse button picks last point.')
but = 1;
while but == 1
   [xi,yi,but] = ginput(1);
   plot(xi,yi,'ro')
   n = n+1;
   xy(:,n) = [xi;yi];
end
% Interpolate with a spline curve and finer spacing.
t = 1:n;
ts = 1:0.1:n;
xys = spline(t,xy,ts);
%plot interpolated curve
plot(xys(1,:),xys(2,:),'b-')
hold off
%plot
%axis off
