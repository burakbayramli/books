%nm115: to plot 3D graph
x=-2:.1:2;  y=-2:.1:2;
[X,Y] = meshgrid(x,y);
Z =X.^2 + Y.^2;
figure(2), clf 
mesh(Z,[-30,40])
pause, figure(1), clf
t = 0:pi/50:6*pi;  expt= exp(-.1*t);
xt= expt.*cos(t); yt= expt.*sin(t);
subplot(221), plot3(xt, yt, t), grid on %helix(³ª¼±Çü)
subplot(222), plot3(xt, yt, t), grid on, view([0 0 1])
subplot(223), plot3(t, xt, yt), grid on, view([1 -3 1])
subplot(224), plot3(t, yt, xt), grid on, view([0 -3 0])
pause, clf
subplot(221), mesh(X,Y,Z), grid on %[azimuth,elevation]=[-37.5,30]
subplot(222), mesh(X,Y,Z), view([0,20]), grid on
pause, view([30,30])
subplot(223), contour(X,Y,Z)
subplot(224), contour(X,Y,Z,[.5,2,4.5])
