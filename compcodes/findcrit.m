

% Function mfile  findcrit
%  Given a function f(x,y) in a mfile, or as an inline function.
%  User must choose a vectors "corners = [a b c d]"
%  which give the corners of a rectangle
%
%                 (a,d)-------------(b,d)
%                   |                 |
%                   |                 |
%                   |                 |
%                 (a,c)-------------(b,c)
%
%       The call is  findmax(f,corners) when f is given as an inline
%  function, and  findmax('f',corners)  when f is given in an mfile.
%  The mfile then finds the maximum and the minimum over a 50 by 50 mesh on
%  this rectangle and displays the level curves of f in this rectangle. User then
%  decides on a smaller rectangle and clicks on the lower left and lower right
%  corners. A third click is next made anywhere. The smaller rectangle is
%  blown up, level curves of f displayed in this smaller rectangle, and 
%  the max and min of f is found over a 50 by 50 mesh. This can be done 4 times.  

function  out = findcrit(f, corners)

a = corners(1); b = corners(2); c = corners(3); d = corners(4);
m = .5*(c+d);
c = m -.4*(b-a);
d = m+ .4*(b-a);

x = linspace(a,b,50);
y = linspace(c,d,50);
[X,Y] = meshgrid(x,y);
Z = feval(f, X,Y);
contour(X,Y,Z);
disp(' ')
      disp('  x          y           fmax       x          y         fmin')
disp('  ')
for n = 1:4
   [xcorners, ycorners]  = ginput(2);
   a = xcorners(1);
   b = xcorners(2);
   c = ycorners(1);
   d = ycorners(2);
   m = .5*(c+d);
   c = m -.4*(b-a);
   d = m +.4*(b-a);
   hold on
   plot([a b b a a], [c c d d c])
   hold off
   ginput(1);
   x = linspace(a,b,50);
   y = linspace(c,d,50);
   [X,Y] = meshgrid(x,y);
   Z = feval(f,X,Y);
   contour(X,Y,Z);
   hold on
            [row,I] = max(Z);
            [fmax, j] = max(row);
            plot([x(j)], [y(I(j))], '*')
            text(x(j), y(I(j)), '\fontsize{14pt}max')

            [row,J] = min(Z);
            [fmin,k] = min(row);
            plot([x(k)], [y(J(k))], '*')
            text(x(k), y(J(k)), '\fontsize{14pt}min')
            [x(j), y(I(j)), fmax, x(k), y(J(k)), fmin]
         
end

hold off



  
