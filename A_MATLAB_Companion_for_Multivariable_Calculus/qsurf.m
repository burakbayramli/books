
%  Function mfile       qsurf (quick surf)
%
%      Slightly faster graphing for numerical functions of two variables.
%   Call is qsurf(f,corners, n) when f is an inline function, and
%   qsurf('f', corners, n) when f is given in an mfile. 
%      corners is the 4 vector [a b c d]. It is assumed that
%   a < b and c < d. f is then graphed over the rectangle 
%   
%                   (a,d)  ----------- (b,d)
%                     |                  |
%                     |                  |
%                     |                  |
%                   (a,c)  ----------- (b,c)
%   
%     The meshgrid is n times n.  When n is omitted in the call,
%  the default value is 50.



 function out = qsurf(f,corners,n)
        if nargin < 3
           n = 50;
        end
        a = corners(1); b = corners(2); c = corners(3); d = corners(4);
        x = linspace(a,b,n+1); y = linspace(c,d,n+1);
        [X,Y] = meshgrid(x,y);
        Z = feval(f,X,Y);
        surf(X,Y,Z)

