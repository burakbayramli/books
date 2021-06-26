
% This function mfile takes as input the three coordinate functions
% of a parameterized curve in 3D. The call is  frenet(x,y,z)
% if x,y,z are given as inline functions, and  frenet('x','y', z'),
% if x,y,z are given in mfiles.  The user then is asked to 
% enter a value of t.  The vectors T,N,B of the frenet frame are 
% calculated at this value of t using difference quotients. The
% vectors are attached to the graph of the curve at the point
%  x(t), y(t), z(t).  This can be done 4 times.  
%  The components of T,N,B are printed as column vectors on the
%  screen as well as the curvature kappa, and the tangential and 
%  normal components of the acceleration.




function w = frenet(x,y,z)
 hold on
 for n = 1:4	 
     t = input('enter a value of  t   ') 
     h = 1e-6;
     p0 = [feval(x, t), feval(y,t), feval(z,t)];
     p1 = [feval(x, t+h), feval(y,t+h), feval(z,t+h)];
     p2 = [feval(x, t-h), feval(y,t-h), feval(z,t-h)];

     v = (p1-p2)/(2*h);
     a = (p1 - 2*p0 + p2)/h^2;

     T = v/norm(v);
     projection = a - dot(a,T)*T;

     N = projection/norm(projection);

     B = cross(T,N);

     kappa = norm(cross(v,a))/norm(v)^1.5;

     arrow3(p0, T, 'r')
     arrow3(p0, N, 'k')
     arrow3(p0, B, 'g')

     text(p0(1) + T(1), p0(2) + T(2), p0(3) + T(3), ' T ')
     text(p0(1) + N(1), p0(2) + N(2), p0(3) + N(3), ' N ')
     text(p0(1) + B(1), p0(2) + B(2), p0(3) + B(3), ' B ')
     disp('      T           N            B')   
     frame = [T', N', B']
     disp('  ')
     disp(' kappa       a_T           a_N  ')
     [kappa, dot(a,T),   dot(a,N)]

end
hold off




function out = arrow3(P,V,color)

if nargin < 3
    color = 'b';
end

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
   Q = [0 0 -1; 0 1 0; 1 0 0];
end

p = Q*upper; q = Q*lower;

plot3([x0+p(1), x0+a], [y0+p(2), y0+b], [z0+p(3), z0+c], color)
plot3([x0+q(1), x0+a], [y0+q(2), y0+b], [z0+q(3), z0+c], color)

if hchek == 0
   hold off
end

