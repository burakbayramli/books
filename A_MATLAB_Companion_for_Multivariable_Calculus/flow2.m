%
%        Function mfile  flow2.m
%
%   Function mfile to compute the deformation of a disk following
%   the flow of a vector field.  The call is
%   flow2(corners, times) where as usual corners = [a,b,c,d]
%   is the vector of corner coordinates of the rectangle where the
%   vector field if displayed. times = [t1 t2 t3 t4] is a vector of
%   four times. The deformed disk is displayed at the times
%   t1, t2, t3,t4.    The vector field must be provided in
%   mfiles u.m and v.m.   The mfile wdot.m is also needed. 
%   t1,t2,t3,t4 are the times at which the deformed disk is 
%   observed. After the call, the program asks user to click on the
%   to place a disk. The image of the disk under the flow is then
%   calculated and plotted using ode45 on the boundary points of the
%   disk. The area of each image is calculated approximately.



function out = flow2(corners, times)
 xmin = corners(1); xmax = corners(2);
 width = xmax - xmin;
 ymin = corners(3); ymax = ymin+.8*width; 

 x = linspace(xmin,xmax,16); y = linspace(ymin,ymax ,16);
 [X,Y] = meshgrid(x,y);

 U = u(X,Y); V = v(X,Y);

quiver(X,Y,U,V)
axis([xmin,xmax, ymin, ymax])
hold on

%center = input('enter coords of center of circle [a,b]  ')
%a = center(1); b = center(2);

disp(' click where you want to the flow to begin   ')
disp('  ')
[a,b] = ginput(1)
r = width/20
n = 20; theta = 0:2*pi/n: 2*pi;

x0 = a + r*cos(theta); y0 = b + r*sin(theta) ;
fill(x0, y0, 'r')

x1 = zeros(1,n); y1 = x1;
x2 = x1; y2 = y1;
x3 = x2; y3 = y2;
x4 = x3; y4 = y3;
tspan = [0 times];

for j = 1:n+1

   w0 = [x0(j), y0(j)];
   [t,w] = ode45('wdot', tspan, w0);
   x1(j) = w(2,1); y1(j) = w(2,2);
   x2(j) = w(3,1); y2(j) = w(3,2);
   x3(j) = w(4,1); y3(j) = w(4,2);
   x4(j) = w(5,1); y4(j) = w(5,2);
end

area0 = 0;
for j = 1:n
    area0 = area0 +.5*(x0(j+1) +x0(j))*(y0(j+1) - y0(j));
end
area0


fill(x1,y1, 'r')
area1 = 0;
for j = 1:n
    area1 = area1 + .5*(x1(j+1) + x1(j))*(y1(j+1) - y1(j));
end
area1
    
fill(x2,y2, 'r')
area2 =0;
for j = 1:n
    area2 = area2 + .5*(x2(j+1) +x2(j))*(y2(j+1) - y2(j));
end
area2

fill(x3,y3, 'r')
area3 = 0;
for j = 1:n
    area3 = area3 + .5*(x3(j+1) + x3(j))*(y3(j+1) - y3(j));
end
area3

fill(x4,y4, 'r')
area4 = 0;
for j = 1:n
  area4 = area4 + .5*(x4(j+1) + x4(j))*(y4(j+1) - y4(j));
end
area4

hold off


