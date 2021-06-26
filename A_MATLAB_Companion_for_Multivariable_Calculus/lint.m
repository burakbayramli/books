%
%         Function mfile     lint.m
% 
%    This mfile estimates the line integral of  a two-dimensional vector 
%    field F = [u(x,y), v(x,y)] along a path C. The call is 
%    lint(u,v,corners) where $u$ and $v$ are functions of x,y  given
%    in mfiles or as inline functions.  corners = [a b c d]
%    is the vector of corner coordinates of a rectangle R. 
%       After the call, user is asked to enter the number of line
%    segments in the path. User then clicks with left mouse
%    button on the figure, until the number of segments is completed
%    The value of the line integral on each segment is displayed on
%    the screen.
%    The program also estimates the integral of xdy along C.
%    When C is a closed polygon, this line integral is the area  of
%    the enclosed polygon.


function out = lint(u,v,corners)

   a = corners(1); b = corners(2); c = corners(3); d = corners(4);
   x = linspace(a,b,11);
   y = linspace(c,d,11);

   [X,Y] = meshgrid(x,y);
   U = feval(u,X,Y);
   V = feval(v,X,Y);
   quiver(X,Y,U,V)
   axis image
   hold on

   N = input('enter the number of segments in your path  ')
   linesum = 0;
   A = 0;
   
   [x0,y0] = ginput(1);

   n = 50; 
   simpvec = 2*ones(1,n+1);
   simpvec(2:2:n) = 4*ones(1,n/2);
   simpvec(1) = 1; simpvec(n+1) = 1;
   for j = 1:N
     [x1, y1] = ginput(1);
     plot([x0,x1], [y0,y1], 'r')

     xm = .4*x0 + .6*x1; ym = .4*y0 + .6*y1;
     d = norm([x1-x0, y1-y0]);
     l = .025*max([b-a, d-c]); 
     delx = (l/d)*(x1-x0); dely = (l/d)*(y1-y0);
     xedge = [xm, xm-delx-dely, xm-delx+dely];
     yedge = [ym, ym-dely+delx, ym-dely-delx];
     fill(xedge, yedge, 'r')
     segment_number = num2str(j);
     text(x1,y1, segment_number)


     dx = (x1-x0)/n; dy = (y1-y0)/n;
     xx = linspace(x0, x1,n+1);
     yy = linspace(y0 ,y1,n+1);
     Fdx = simpvec*feval(u,xx,yy)'*dx/3;
     Fdy = simpvec*feval(v,xx,yy)'*dy/3;    
     line_integral = Fdx + Fdy
     linesum = linesum + line_integral;
     dA = .5*(x1+x0)*(y1-y0);
     A = A +dA;
     x0 = x1; y0 = y1;
   end
   
   area = A
   total_line_integral = linesum
   hold off
