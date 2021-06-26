function out = impl(f, corners,c)

% This graphing function displays a level surface f(x,y,z) = c .  
% Originally written by Jonathan Rosenberg, Department of Mathematics,
% University of Maryland.
% The call is impl(f,corners, c) when f is given as an inline function, and
% impl('f',corners,c) when f is given in an mfile.  Here 'corners'
% is a 6 vector % [xmin, xmax, ymin, ymax, zmin, zmax] which
% defines the domain in % which the level surface is sought. 
% 'c' is, of course, the  value. 
%  The max and min of the function of the function over this domain
% is estimated. If the value c does not fall in this range, the program
% stops.  


xmin = corners(1); xmax = corners(2);  ymin = corners(3); ymax = corners(4);
zmin = corners(5); zmax = corners(6);

x = linspace(xmin, xmax ,21); y = linspace(ymin,ymax,21);
z = linspace(zmin,zmax,21);
[XX,YY,ZZ] = meshgrid(x,y,z);
W = feval(f, XX,YY,ZZ);
m = min(min(min(W)));
M = max(max(max(W)));
sprintf('The max over this domain is %5.5f%', M)
sprintf('The min over this domain is %5.5f%', m)

if c < m | c > M
sprintf('In this domain, the function does not take on the value %5.5f%', c)
else 
   
[X,Y] = meshgrid(x,y);
dz = (zmax-zmin)/40;
   for zz = zmin:dz:zmax
      Z = feval(f,X,Y,zz);
      con = contours(X,Y,Z, [c,c]);
      nn = size(con,2);
      if nn > 0
         j = 1;
         while j < nn
            npairs = con(2,j);
            xdata = con(1, j+1: j+npairs);
            ydata = con(2, j+1: j+npairs);
            plot3(xdata, ydata, zz+ 0*xdata)
            j = j+npairs +1;
            hold on
         end
      end
   end
end 
axis(corners)
xlabel('x')
ylabel('y')
zlabel('z')
hold off
