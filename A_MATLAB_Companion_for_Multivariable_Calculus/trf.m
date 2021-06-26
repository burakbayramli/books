

%              Function trf (transformation)
%   
%   This mfile plots a rectangle with subrectangles in the left
%   figure window, and its curvilinear image with images of the 
%   subrectangles in the right figure window.  The transformation
%   is (u,v) - > (x,y) = (f(u,v), g(u,v)). 
%       The call is trf(f,g,corners) when f and g are given as
%   inline functions, and trf('f', 'g',corners) when f and g
%   are given in mfiles. 
%       corners = [a b c d] is a vector of corner coordinates of
%   the rectangle a < x < b,  c < y <d. 
%       After the call the user is asked to enter the number n of
%   subdivisions in the x direction, and the number m of subdivisions
%   in the y direction. The graphs are drawn and the program pauses.
%   To continue the user hits return. Then the affine images of the
%   subrectangles are plotted in the right figure and the sums of 
%   their areas is computed. This gives an approximation to the
%   area of the curvilinear region in the right figure.

function out = trf(f,g,corners)

subdiv = input(' enter the number of subdivisions [n m]  ')

a =  corners(1); b = corners(2); c = corners(3); d = corners(4);
n = subdiv(1); m = subdiv(2);

t = linspace(a,b,101); s = linspace(c,d,101);
u = linspace(a,b, n+1); v = linspace(c,d,m+1);

subplot(1,2,1)
%  fill([a a+.01*(b-a), a+.01*(b-a), a], [c,c,d,d],'r') 
  plot([a a], [c d], 'r')
  hold on
  for j = 2:n+1
     plot(u(j) +0*s, s,'k')
     hold on
  end
 % fill([a b b a], [c c c+.015*(d-c), c+.015*(d-c)], 'b')
  plot([a b], [c c], 'b')
  for i = 2:m+1
     plot(t, v(i)+0*t, 'k')
  end
  hold off
  xlabel( 'u' )
  ylabel( 'v' )
  axis equal

subplot(1,2,2)

  plot(feval(f, u(1),s), feval(g,u(1),s), 'r')
  hold on

  for j = 2:n+1
    plot(feval(f,u(j), s), feval(g,u(j), s), 'k')
    hold on
  end
  plot(feval(f, t,v(1)), feval(g,t,v(1)), 'b')
  for i = 2:m+1
    plot(feval(f, t,v(i)), feval(g,t,v(i)), 'k') 
    hold on
  end
  axis equal

  disp('Hit return to continue ')
  pause
  A = 0;
  du = (b-a)/n; dv = (d-c)/m; h = 10^(-6);
  umid = linspace(a +.5*du, b - .5*du, n);
  vmid = linspace(c +.5*dv, d - .5*dv, m);
  hold on
  for i = 1:m
     for j = 1:n
         uu = umid(j); vv = vmid(i);
         fu = .5*(feval(f,uu+h, vv) - feval(f, uu-h, vv))/h;
         fv = .5*(feval(f,uu,vv+h) - feval(f, uu, vv-h))/h;
         gu = .5*(feval(g,uu+h,vv)  - feval(g, uu-h,vv))/h;
         gv = .5*(feval(g,uu, vv+h) - feval(g, uu, vv-h))/h;
         J = [fu, fv; gu, gv];
         p = J*[du/2,0]';  q = J*[0 dv/2]';

         xplot = [p(1)+q(1), -p(1)+q(1), -p(1)-q(1), p(1)-q(1)] +feval(f,uu,vv);
         yplot = [p(2)+q(2), -p(2)+q(2), -p(2)-q(2), p(2)-q(2)] +feval(g,uu,vv);
         fill(xplot, yplot, 'g')

         A = A + abs(det(J))*du*dv;
     end
  end
  xlabel('x')
  ylabel('y')
  axis equal
  hold off

  A       


