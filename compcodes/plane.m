
%  This mfile graphs a piece of a plane in 3D space.  The complete 
%call is  
%                 plane(P,N,width, height)
%
%    Here P = [x0,y0,z0] is a point contained in the plane, and 
% N = [a,b,c] is the normal vector to the plane.  The third and
% fourth arguments are optional. If the call is  simply  plane(P,N),
% the piece of the plane is a square of side 2, centered at P.
% The call  plane(P,N,a,b)   produces a piece of the plane which is
% by 2b by 2b centered at P.  




function z = plane(P,N, width, height)
if nargin < 3
   width = 1; height= 1;
end
    x0 = P(1);
    y0 = P(2);
    z0 = P(3);
    N = N/norm(N);
    a = N(1); b = N(2); c = N(3);


    s = - width: .1*width : width;  t = -height: .1*height: height;

    [S,T] = meshgrid(s,t);
    hhchek = ishold;
    arrow3(P,N,'r')
    hold on
    arrow3(P-.3*N,.3*N,'r') 

    r = sqrt(a^2 +b^2);

    if r > 0
       v = [b/r, -a/r, 0];
       w = [-a*c/r, -b*c/r, r];

    else
       v = [1 0 0];
       w = [0 -1 0];
    end

    X =  x0 + v(1)*S +w(1)*T;
    Y =  y0 + v(2)*S +w(2)*T;
    Z =  z0 + v(3)*S +w(3)*T;

    low = min(min(Z));
    high = max(max(Z));



    surf(X,Y,Z);
    colormap(gray);
    caxis([low-6, high]);
    axis equal

    if hhchek == 0
       hold off
    end







function out = arrow3(P,V,color)

x0 =P(1); y0 = P(2); z0 = P(3);
a = V(1); b = V(2); c = V(3);
l = max(norm(V), eps);

x = [x0 x0+a]; y = [y0 y0+b]; z = [z0 z0+c];
hchek = ishold;
plot3(x,y,z,color)
hold on

h = l - min(.2*l, .2) ;
v = min(.2*l/sqrt(3), .2/sqrt(3) );

upper = [h, v*tan(pi/6), 0]';
lower = [h, -v*tan(pi/6), 0]';

r = sqrt(a^2 +b^2);
if r > 0
 
   col1 = [a b c]/l;
   col2 = [-b/r, a/r, 0];
   col3 = [-a*c/(l*r), -b*c/(l*r), r/l];

   Q = [col1; col2; col3]' ;

else
   if c > 0
       Q = [0 0 -1; 0 1 0; 1 0 0];
   else
       Q = [0 0 1; 0 1 0; -1 0 0];
   end
end

p = Q*upper; q = Q*lower;

plot3([x0+p(1), x0+a], [y0+p(2), y0+b], [z0+p(3), z0+c], color)
plot3([x0+q(1), x0+a], [y0+q(2), y0+b], [z0+q(3), z0+c], color)

if hchek == 0
   hold off
end

