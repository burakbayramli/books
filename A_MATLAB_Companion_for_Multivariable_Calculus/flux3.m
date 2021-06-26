%
%           Function mfile     flux3.m
%
%     The call is   flux3(u,v,w)  where F = [u,v,w] is a vector field
%     on the cube of side 2 centered at (0,0,0).  u,v,w are function of
%     x,y,z written in mfiles or given as inline functions. 
%         At the call the vector field is displayed in the upper figure.
%     The lower figure displays a projection of the unit sphere. The
%     coordinate angles are theta= longitude, and alpha = latitude
%     (angle from the equator). 
%         The program pauses for user to enlarge the window if 
%     desired, and continues when return is hit. Then the user
%     clicks on the lower figure.  In the upper figure a plane through
%     the origin is displayed with normal determined by the angles
%     in the  lower figure. 
%          n = [cos(theta)cos(alpha), sin(theta)cos(alpha), sin(alpha) ]
%     The program then uses the two dimension Simpson's rule to
%     estimate the flux of the vector field through this piece of
%     surface.  This may be done 5 times.










function  out= flux3(u,v,w)


[X,Y] = meshgrid(-.75:.5:.75);
subplot(2,1,1)
   for z = -.75:.5:.75
      Z = z+0*X;
      U = feval(u,X,Y,Z);
      V = feval(v,X,Y,Z);
      W = feval(w,X,Y,Z);
      quiver3(X,Y,Z,U,V,W)
      hold on
   end
   arrow3([0 0 -1.1], [1.2,0 0],'r')
   arrow3([0 0 -1.1], [0 1.2 0], 'r')
   view(135, 40)
   text(0, 2, -1.1, '\fontsize{12pt}y')
   text(2.1, 0, -1.1, '\fontsize{12pt}x')
   axis image

m = 100;
a = .75;
s = linspace(-a, a, m+1); t = s;

[S,T] = meshgrid(s,t);

subplot(2,1,2)
%  set(gca, 'position', [.13, .11, .7750 .25])
  theta = linspace(-pi, pi, 101);
  alpha = linspace(-pi/2, pi/2);
  thth = linspace(-pi, pi, 9);
  for j = 1:9
     plot(thth(j)*(1-(2*alpha/pi).^2), alpha)
     hold on
  end

  plot(theta, 0*theta)
  plot(.75*theta, 0*theta + pi/4)
  plot(.75*theta, 0*theta - pi/4)

  text(-3.3, -.15, '-\pi')
  text(-pi/2 -.1, -.15, '-\pi/2')
  text(0, -.15, ' 0 ')
  text(pi/2, -.15, '\pi/2')
  text(pi, -.15, '\pi')
  
  text(0, pi/2 + .15, '\pi/2')
  text(0, pi/4 - .15, '\pi/4')
  text(0, -pi/4 -.15, '-\pi/4')
  text(0, -pi/2-.15,  '-\pi/2')

  text(3*pi/4 + .1, -.25, '\fontsize{12pt}\theta')
  text(0, pi/4+ .35, '\fontsize{12pt}\alpha ')
  
  arrow([0 0], [pi,0], 'r')
  arrow([0 0], [0, pi/2], 'r')
  axis off
  pause

svec =  2*ones(1,m+1);
svec(2:2:m) = 4*ones(1,m/2);
svec(1) = 1; svec(m+1) = 1;
Sp = svec'*svec;

 disp('   ')
 disp('  angle theta      angle alpha      flux ')

for j = 1:5
subplot(2,1,2)
   [x, y] = ginput(1);
   plot([x], [y], '*')

   th = x/(1-(2*y/pi)^2); alpha = y;

   if alpha == pi/2
      n = [0 0 1];
      uu = [1 0 0];
      vv = [0 1 0];
   elseif alpha == -pi/2
      n = [0 0 -1];
      uu = [1 0 0];
      vv = [0 -1 0];
   else
       n = [cos(th)*cos(alpha), sin(th)*cos(alpha), sin(alpha)];
       uu = [-n(2), n(1), 0];
       uu = uu/norm(uu);
       vv = cross(n,uu);
   end
   XX = uu(1)*S + vv(1)*T;
   YY = uu(2)*S + vv(2)*T;
   ZZ = uu(3)*S + vv(3)*T;

   xedge = [XX(1,1), XX(1,m+1), XX(m+1,m+1), XX(m+1,1)];
   yedge = [YY(1,1), YY(1,m+1), YY(m+1,m+1), YY(m+1,1)];
   zedge = [ZZ(1,1), ZZ(1,m+1), ZZ(m+1,m+1), ZZ(m+1,1)];

   subplot(2,1,1)
     if j > 1
        delete(h1)
        delete(h2)
        delete(h3)
     end
     h1 = fill3(xedge, yedge, zedge, 'c');
     M = [-.05*uu; .05*uu; .05*uu+.5*n; .08*uu+.5*n;...
                                  .65*n; -.08*uu+.5*n; -.05*uu+.5*n];
     h2 = fill3(M(:,1), M(:,2), M(:,3), 'r');
     N = [-.05*vv; .05*vv; .05*vv+.5*n; .08*vv+.5*n; ...
                                   .65*n; -.08*vv+.5*n; -.05*vv+.5*n];
     h3 = fill3(N(:,1), N(:,2),N(:,3),'r');
     axis image

   UU = u(XX,YY,ZZ);
   VV = v(XX,YY,ZZ);
   WW = w(XX,YY,ZZ);

   simpsum = n(1)*sum(sum(Sp.*UU))+n(2)*sum(sum(Sp.*VV))+n(3)*sum(sum(Sp.*WW));
   result = (2*a/(3*m))^2 *simpsum;
   sprintf('    %2.4f          %2.4f        %2.4f',th,alpha,result)

end
subplot(2,1,1)
   hold off
subplot(2,1,2)
   hold off
hold off




function y = arrow(P,V,color)

if nargin < 3
   color = 'b';
end
x0 = P(1); y0 = P(2);
a = V(1); b = V(2);

l = max(norm(V), eps);
u = [x0 x0+a]; v = [y0 y0+b];
hchek = ishold;

plot(u,v,color)
hold on
h = l - min(.2*l, .2) ; v = min(.2*l/sqrt(3), .2/sqrt(3) );

a1 = (a*h -b*v)/l;
b1 = (b*h +a*v)/l;

plot([x0+a1, x0+a], [y0+b1, y0+b], color)

a2 = (a*h +b*v)/l;
b2 = (b*h -a*v)/l;   

plot([x0+a2, x0+a], [y0+b2, y0+b], color)
if hchek == 0
hold off
end


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

