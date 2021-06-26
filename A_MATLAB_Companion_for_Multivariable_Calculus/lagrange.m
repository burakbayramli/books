%  Function mfile,  lagrange 
%
%    Given a function f(x,y) in an mfile or as an inline function.
%    Given a constraint function g(x,y) in the same way.
%    User provides a vector corners = [a b c d] which define the
%    corners of a rectangle. 
%          
%                  (a,d)-----------(b,d)
%                    |               |
%                    |               |
%                    |               |
%                  (a,c)-----------(b,c)
%
%   The rectangle should be chosen to include the level curve 
%   g(x,y) = 0.
%         The call is lagrange(f,g,corners) for f and g given as inline 
%   functions, and lagrange('f', 'g', corners)  for f and g given in
%   mfiles.
%          User then clicks on points on this zero level curve
%   of g.  The level curve of f through point is displayed along with
%   the value of f on this curve. This can be done 5 times.

function out = lagrange(f,g,corners)

a = corners(1);
b = corners(2);
c = corners(3);
d = corners(4);

x = linspace(a,b,50);
y = linspace(c,d,50);

[X,Y] = meshgrid(x,y);

Z = feval(f,X,Y);
W = feval(g,X,Y);
disp(' ')
disp('  x          y         f(x,y)  ')
disp('  ')

contour(X,Y,W, [0 0], 'k')
axis equal
hold on

for n = 1:5
   [p,q] = ginput(1);
   z0 = feval(f, p,q);
   contour(X,Y,Z, [z0 z0], 'r')
   z1 = num2str(z0);
   text(p,q, z1)
   [p,q,z0]
end
 xlabel('x axis ')
 ylabel('y axis ')  

hold off


