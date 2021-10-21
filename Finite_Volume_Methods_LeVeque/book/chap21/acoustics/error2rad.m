%  error2rad.m
%  compute errors in 2d solution by comparing to 1d radially symmetric
%  solution in the subdirectory 1drad

% assumes Frame is set to the frame desired, which agrees with the time of
% the same frame in subdirectory 1drad.

figure(1)
cd 1drad
plotstyle = 'b-';
plotframe1
cd ..
hold on

q1d = coarsen(q,10)';
r1d = coarsen(x,10);

PlotType = 4;
plotframe2
hold on
plot(r1d,q1d,'r')
hold off

%[x2,y2] = meshgrid(x,y);
%x2 = x2';
%y2 = y2';

q2 = make2drad(x0,y0,r1d,q1d,xp,yp);

err = q - q2;
err1 = sum(sum(abs(err))) * dx*dy
errmax = max(max(abs(err)))

figure(2)
pcolor(xp,yp,err)
shading flat
axis square
colorbar
title('Error')
figure(1)
