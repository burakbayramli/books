
%                 Function tsurf  (triangular surface)
%  [X,Y] is a meshgrid of points in rectangle R. The heights of
%  the surface at the meshpoints are given in a matrix Z.
%  Frequently Z will be computed by a function Z = f(X,Y).  
%  The call tsurf(X,Y,Z) calculates the area of the surface
%  consisting of triangles spanning the points (xj,yi,f(xj,yi)).
%  The surface consisting of triangles is also graphed. 

function  out = tsurf(X,Y,Z)
   
     n = size(X,2)-1;  m = size(Y,1)-1;

     a = X(1,1); b = X(1,n+1); c = Y(1,1); d = Y(m+1,1);

     dx = (b-a)/n; dy = (d-c)/m;
     A = 0;

     for i = 1:m
         for j = 1:n
             x1 = X(i,j); x2 = x1+dx; 
             y1 = Y(i,j); y2 = y1+dy;
             z1 = Z(i,j); z2 = Z(i,j+1);
             z3 = Z(i+1, j+1); z4 = Z(i+1,j);
             xedge = [x1 x2 x1];
             yedge = [y1 y1 y2];
             zedge = [z1 z2 z4];
             fill3(xedge,yedge, zedge, 'b')
             hold on
             xedge = [x2 x2 x1];
             yedge = [y1 y2 y2];
             zedge = [z2 z3 z4];
             fill3(xedge, yedge, zedge, 'c')
             dAlower = .5*sqrt(dx^2*dy^2 + (z2-z1)^2*dy^2 + (z4-z1)^2*dx^2);
             dAupper = .5*sqrt(dx^2*dy^2 + (z4-z3)^2*dy^2 + (z2-z3)^2*dx^2);
          A = A + dAlower + dAupper;
          end
      end
      out= A;

      hold off


            
             
