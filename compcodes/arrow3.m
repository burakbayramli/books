


%  this mfile plots an arrow in 3D with tail at the point (x0, y0, z0) and
%  tip at the point  (x0+a, y0+b, z0+c).  Writing P = [x0, y0, z0] and 
%  V = [a,b,c], the complete call is   arrow3(P,V, color).  The third argument,
%  color, is optional.  If the call is simply   arrow3(P,V), the
%  arrow will be blue.  To get a red arrow, use  arrow3(P,V, 'r').



function out = arrow3(P,V,color)

if nargin < 3
    color = 'b';
end

x0 =P(1); y0 = P(2); z0 = P(3);
a = V(1); b = V(2); c = V(3);
l = max(norm(V), eps);

x = [x0 x0+a];
y = [y0 y0+b];
z = [z0 z0+c];
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

