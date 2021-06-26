%                Script file    newton2.m
%   This script implements newton's method in two dimensions.
%   Required input is the functions f(x,y) and g(x,y), and the
%   Jacobian J(x,y).  These should be provided in mfiles f.m,
%   g.m and bigj.m .
%   The script asks for a first guess p1, which should be entered
%   as a column vector.  The script then plots this point, and
%   pauses before computing the next point p2. After hitting
%   return, the script computes p2, plots it, and pauses, waiting
%   for user to hit return for the next iterate p3. Iterates
%   p1, ..., p5 are computed in this fashion.  The numerical values
%   are displayed on the screen. 
%       It is very useful to use the contour command to plot 
%   the zero level curves of f and g to approximately locate the
%   root. Then hold on so that the iterates are plotted on the
%   contour map.

p = input('enter the starting value p = [x1;y1]  ');

x = p(1); y = p(2); 
disp('   ')
disp('  iterate      x            y           f(x,y)       g(x,y)   ')
ff = f(x,y); gg = g(x,y);

fprintf('   %d      %0.5f      %0.5f      %0.5f     %0.5f\n',1,x,y,ff,gg)

plot([x], [y], '*')
hold on
disp('hit return to continue')
pause

for n = 2:5
    p  = bigj(x,y)\(bigj(x,y)*p - [f(x,y);g(x,y)]);
    x = p(1); y = p(2);
    ff = f(x,y); gg = g(x,y);
    fprintf('   %d      %0.5f      %0.5f      %0.5f     %0.5f\n',n,x,y,ff,gg)
    plot([x], [y], '*')
    if n < 5 
        disp(' hit return to continue ')
        pause
    end
end

hold off




