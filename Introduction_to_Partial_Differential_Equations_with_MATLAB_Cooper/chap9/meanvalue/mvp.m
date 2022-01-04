
%                   Program mvp (mean value property)
% 
%  Computes the values of three different functions, u, v, and w in
%  the square G of side one. User enters the coordinates of the center
%  of a circle, and its radius, making sure that the circle fits inside
%  the square. The program computes the average of the function around
%  the circle, and prints this value as well as the value at the center
%  of the circle on the screen.  The graph of the function is plotted
%  together with the circle. The values of the function around the circle 
%  are put in the vector trace, and can be plotted against the vector
%  theta. The value at the center is put in the constant vector valctr.


m = input('Enter the choice of function 1, 2, or 3     ')

center = input('Enter the center of the circle in the form [a,b]     ')

rho = input('Enter the radius of the circle    ')

deltheta = pi/25;

theta = 0: deltheta: 2*pi;

xx = center(1) + rho*cos(theta);
yy = center(2) + rho*sin(theta);

[X,Y] = meshgrid(0:.05:1);

if m == 1
  trace = u(xx,yy);
  Z = u(X,Y);
  centervalue = u(center(1), center(2));
elseif m == 2
  trace = v(xx,yy);
  Z = v(X,Y);
  centervalue = v(center(1), center(2));
else
  trace = w(xx,yy);
  Z = w(X,Y);
  centervalue = w(center(1), center(2));
end

valctr = centervalue*ones(1,51);

% use Simpson's rule to calculate the average around the circle.

  sum1 = sum(trace(1:2:49));
  sum2 = sum(trace(2:2:50));

  average =(.5/pi)*( (2*deltheta/3)*sum1 + (4*deltheta/3)*sum2)

  fprintf(' The average around the circle is  %g.\n',average)

  fprintf(' The value at the center is  %g.\n',centervalue )


  plot3(xx,yy,zeros(1,51), 'r')
  
  axis([0 1 0 1 0 4])

  hold on
  plot3(xx,yy,trace, 'g')

  mesh(X,Y,Z);
  colormap(gray)
  brighten(.9)
  view(45,30)

  hold off
