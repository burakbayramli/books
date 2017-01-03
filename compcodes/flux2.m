%
%             Function mfile      flux2.m
%
%    The call is flux2(u,v,corners).  u = u(x,y) and v = v(x,y) are
%    the components of a vector field F = [u,v]. u and v are to be
%    written as mfiles or as inline functions. corners [a,b,c,d]
%    is the vector of corner coordinates of a rectangle R. 
%         After the call, the user must enter the number of line 
%    segments that will comprise the path, which will usually be
%    the boundary of a polygon. When the path goes around the
%    polygon in a counter clockwise direction, the normal n points out.
%             User then clicks with left mouse
%    button on the figure, until the number of segments is completed
%          The mfile then estimates the flux integral of F around the
%    path C  and the area of the enclosed polygon.


function out = flux2(u,v,corners)

   a = corners(1); b = corners(2); c = corners(3); d = corners(4);

   N = input('enter the number of segments in the path  ')

   x = linspace(a,b,11);
   y = linspace(c,d,11);

   [X,Y] = meshgrid(x,y);
   U = feval(u,X,Y);
   V = feval(v,X,Y);
   quiver(X,Y,U,V)
   axis image
   hold on

   sumflux = 0;
   A = 0;
   
   [x0,y0] = ginput(1);

   n = 50; 
   simpvec = 2*ones(1,n+1);
   simpvec(2:2:n) = 4*ones(1,n/2);
   simpvec(1) = 1; simpvec(n+1) = 1;

   for j = 1:N
     [x1, y1] = ginput(1);
     plot([x0,x1], [y0,y1], 'r')

     xm = .5*x0 + .5*x1; ym = .5*y0 + .5*y1;
     denom = norm([x1-x0, y1-y0]);
     normal = [y1-y0, x0-x1]/denom;
     l = .025*max(b-a,d-c);
     xtip = xm +3*l*normal(1); ytip = ym + 3*l*normal(2);
     plot([xm, xtip], [ym, ytip], 'r')

     nx = l*normal(1); ny = l*normal(2);
     xedge = [xtip, xtip-nx+ny, xtip-nx-ny];
     yedge = [ytip, ytip-ny-nx, ytip-ny+nx];
     fill(xedge, yedge, 'r')
     segment_number = num2str(j);
     text(x1,y1, segment_number)


     dx = (x1-x0)/n; dy = (y1-y0)/n;
     xx = linspace(x0, x1,n+1);
     yy = linspace(y0 ,y1,n+1);
     f1 = simpvec*feval(u,xx,yy)'*dy/3;
     f2 = simpvec*feval(v,xx,yy)'*dx/3;   
     flux = f1 - f2
     sumflux = sumflux + flux;
     A = A + .5*(x1 + x0)*(y1 - y0);
     x0 = x1; y0 = y1;
   end
   area = A
   total_flux = sumflux
   hold off
