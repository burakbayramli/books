

%             Function mfile mslice (multislice)
%  Input is a function f(x,y) in an mfile, or as an inline function
%  and a point P = (x0, y0).  The call is mslice(f,P) when f is
%  given as an inline function, and mslice('f', P) when f is given
%  in a mfile. 
%      When mslice is called, the graph of f
%  near P is displayed in the upper figure. Then user has the 
%  opportunity to resize the figure window. User then must hit return.
%  Lower figure appears with several level curves of f and the point P.
%  User then clicks with right mouse button on the lower figure in
%  different directions around P. Corresponding slice appears in the
%  upper figure. The direction u and the directional derivative Duf
%  are displayed on the screen.  This can be done 5 times.



function out = mslice(f,P)

global  z0 z1 z2

x0 = P(1); y0 = P(2);

x = x0-1:.05:x0+1;
y = y0-1:.05:y0+1;
[X,Y] = meshgrid(x,y);
Z = feval(f,X,Y);

z0 = feval(f,x0,y0);
z1 = max(max(Z));
z1 = max([z1,z0+1,0]);
z2 = min(min(Z));
z2 = min([z2,z0-1,0]);


subplot(2,1,1)
surf(X,Y,Z); shading interp; colormap(gray)
xlabel(' x axis ')
ylabel(' y axis ')
zlabel(' z axis ')
disp('This is a chance to enlarge the window, and rotate the figure ')
disp('When you are finished, enter return ')
rotate3d on
pause
rotate3d off
disp('  ')
disp('    u1        u2        Duf    ')
disp('  ')
format compact

ylower = y0-.5:.02:y0+.5;
[XX,YY] = meshgrid(x,ylower);
ZZ = feval(f,XX,YY);

subplot(2,1,2)
   contour(XX,YY,ZZ, [.5*z0, z0, 1.5*z0], 'k')
   hold on
   plot([x0], [y0], '*')
   axis equal
   xlabel(' x axis ')
   ylabel(' y axis ')
   text(x0+.1, y0+.1, 'P')


for j = 1:5
   subplot(2,1,2)
      [q1 q2] = ginput(1);
      Q = [q1 q2];
      u = (Q-P)/norm(Q-P);
      arrow(P,Q-P)
   subplot(2,1,1)
      surf(X,Y,Z); shading flat; colormap(gray)
      hold on
      myslice(f, P, u)
      xlabel(' x axis ')
      ylabel(' y axis ')
      zlabel(' z axis ')
end
subplot(2,1,1)
hold off
subplot(2,1,2)
hold off
format loose



function out = myslice(f, P, u)
global a b c d z0 z1 z2

ztip = (z1 +z2)/2;

u = u/norm(u);
u1 = u(1); u2 = u(2);

x0 = P(1); y0 = P(2);

x1 = x0 -u1; 
x2 = x0 + u1;
y1 = y0-u2;
y2 = y0+u2; 

xedge = [x1 x0+.6*u1 x0+.9*u1 x0+.6*u1 x1];
yedge = [y1 y0+.6*u2 y0+.9*u2 y0+.6*u2 y1];
zedge = [z1 z1  ztip z2 z2];
fill3(xedge, yedge, zedge, 'c')
hold on
plot3([x0,x0], [y0,y0], [z1,z2],'r')
xx = [x0+.6*u1, x0+.7*u1, x2, x0+.7*u1, x0+.6*u1, x0+.9*u1];
yy = [y0+.6*u2, y0+.7*u2, y2, y0+.7*u2, y0+.6*u2, y0+.9*u2];
zz = [z1,z1,ztip,z2,z2, ztip];
fill3(xx,yy,zz, 'r')

hold off
h = 1e-6;
delz = feval(f, P(1) +h*u(1), P(2)+h*u(2)) -z0;
Duf = delz/h;
[u, Duf] 



function out = arrow(P,V,color)
if nargin < 3
   color = 'b';
end

x0 = P(1); y0 = P(2);
a = V(1); b = V(2);

l = max(norm(V), eps);

u = [x0 x0+a]; v = [y0 y0+b];
hchek = ishold;
plot(u,v,color)
hold on

h = l - min(.2*l, .2) ; v = min(.2*l/sqrt(3), .2/sqrt(3) );

a1 = (a*h -b*v)/l;
b1 = (b*h +a*v)/l;

plot([x0+a1, x0+a], [y0+b1, y0+b], color)

a2 = (a*h +b*v)/l;
b2 = (b*h -a*v)/l;   

plot([x0+a2, x0+a], [y0+b2, y0+b], color)

if hchek == 0
  hold off
end












   
