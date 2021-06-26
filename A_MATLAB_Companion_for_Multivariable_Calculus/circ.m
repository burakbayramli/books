%
%                   Function mfile   circ.m
%
%      The call is    circ(u,v,w,r)   where the vector field 
%   F = [u,v,w] and u,v,w are functions of x,y,z. r is the radius of
%   a disk that passes through the origin. You must have r < 1. 
%       At the call, the vector field is displayed in the upper figure
%   in the cube of side 2, centered at the origin. In the lower sphere
%   is displayed a projection of the unit sphere with coordinated 
%   angles theta (longitude) and alpha (latitude - angle from the equator).
%       The program pauses to allow the user to enlarge the window is
%   desired. Hit return to continue. User then clicks on the sphere
%   to pick out angles theta and alpha.  This determines a unit normal
%      n = [cos(theta) cos(alpha), sin(theta)cos(alpha), sin(alpha) ]
%   A disk of radius r with normal n is displayed in the upper figure
%   and the circulation of the vector field around the disk in the
%   direction indicated by the curved arrow is calculated. The angles,
%   the circulation and the circulation/area are displayed on the
%   screen.

function  out= circ(u,v,w,r)


[X,Y] = meshgrid(-.75:.5: .75);
 subplot(2,1,1) 
      for z = -.75:.5:.75
      Z = z+0*X;
      U = feval(u,X,Y,Z);
      V = feval(v,X,Y,Z);
      W = feval(w,X,Y,Z);
      quiver3(X,Y,Z,U,V,W)
      hold on
   end
   arrow3([0 0 -1], [1.2,0 0],'r')
   arrow3([0 0 -1], [0 1.2 0], 'r')
   view(135, 40)
   text(0, 2.0, -1.1, '\fontsize{12pt}y')
   text(2.1, 0, -1.1, '\fontsize{12pt}x')
   axis image

m = 200;
t = linspace(0, 2*pi, m+1); dt = 2*pi/m;

subplot(2,1,2)
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

 disp('   ')
 disp('  angle theta      angle alpha      circulation      circ/area ')

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

   T  = r*(uu'*cos(t) +vv'*sin(t));
   xx = T(1,:);  yy= T(2,:); zz= T(3,:);

   S = r*(uu'*(-sin(t)) +vv'*cos(t));
   dxx = S(1,:); dyy = S(2,:); dzz = S(3,:); 
   udx = feval(u,xx,yy,zz).*dxx;
   vdy = feval(v,xx,yy,zz).*dyy;
   wdz = feval(w,xx,yy,zz).*dzz;

   circ = svec*(udx +vdy+wdz)'*dt/3;

   subplot(2,1,1)
     if j > 1
        delete(h1)
        delete(h2)
        delete(h3)
        delete(h4)
     end
     h1 = fill3(xx,yy, zz, 'c');
     M = [-.03*uu; .03*uu; .03*uu+.5*n; .08*uu+.5*n;...
                                  .65*n; -.08*uu+.5*n; -.03*uu+.5*n];
     h2 = fill3(M(:,1), M(:,2), M(:,3), 'r');
     N = [-.03*vv; .03*vv; .03*vv+.5*n; .08*vv+.5*n; ...
                                   .65*n; -.08*vv+.5*n; -.03*vv+.5*n];
     h3 = fill3(N(:,1), N(:,2),N(:,3),'r');

     R = [.95*T(:, 1:26), T(:,26), .95*T(:,30), .85*T(:, 26), .9*fliplr(T(:,1:26))];
     h4 = fill3(R(1,:), R(2,:), R(3,:), 'r');

     axis image


     sprintf('    %2.4f          %2.4f        %2.4f         %2.4f',...
                                                        th,alpha,circ, circ/(pi*r^2))

end
subplot(2,1,1)
   hold off
subplot(2,1,2)
   hold off
hold off






