% mainparallel.m
Px = [1:10]'
Py = [ 0.2 1.0 2.6 3.6 4.9 5.3 6.5 7.8 8.0 9.0]'
Qx = [ 1.5 2.6 3.0 4.3 5.0 6.4 7.6 8.5 9.9 ]'
Qy = [ 5.8 7.2 9.1 10.5 10.6 10.7 13.4 14.2 14.5]'
A = [ones(size(Px))  zeros(size(Px)) Px Py
    zeros(size(Qx))  ones(size(Qx)) Qx Qy ]
[c, n] = clsq(A,2)
clf; hold on;
axis([-1 11 -1 17])
plotline(Px,Py,'o',c(1),n,'-')
plotline(Qx,Qy,'+',c(2),n,'-')
hold off;
