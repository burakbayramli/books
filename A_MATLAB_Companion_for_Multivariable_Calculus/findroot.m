%
%        Function mfile   findroot
%  Findroot is an iteractive Newton's method in two dimensions.
%  User provides two functions f(x,y) and g(x,y) in mfiles, or as 
%  inline functions. The system to solve is 
%            f(x,y) = 0      g(x,y) = 0   
% User also provides a vectors corners = [a b c d] of coordinates
% which determine a rectangle where the desired root is sought.
% 
%                  (a,d)-------------(b,d)
%                   |                 |
%                   |                 |
%                   |                 |
%                 (a,c)-------------(b,c)
%
%     The call is  findroot(f,g,corners) when f and g are 
% inline functions, and  findroot('f', 'g', corners) when f and g
% are given in mfiles. 
%   The program displays the zero level curves of f and g. User then
% clicks at a point p0 which is the starting point of the Newton
% iteration process. After the click, the program computes the
% tangent plane approximations to f and g at p0, and displays the
% lines in the x y plane where these planes intersect the plane z = 0.
% The intersection of these two lines is the next point p1 of the
% iteration process.  User can click there, and the process is
% repeated.  Three iterations p1, p2, and p3 can be found this way.



function out = findroot(f,g,corners)

xmin = corners(1); xmax = corners(2);
ymin = corners(3); ymax = corners(4);

width = xmax - xmin;
bit= width/50;

x = linspace(xmin, xmax, 51);
y = linspace(ymin, ymax, 51);

[X,Y] = meshgrid(x,y);

h = 1e-6;
Zf = feval(f,X,Y);
Zg = feval(g,X,Y);
contour(X,Y,Zf, [0,0], 'b')
hold on
contour(X,Y,Zg, [0,0], 'g')

[a,b] = ginput(1);
f0 = feval(f,a,b); g0 = feval(g,a,b);
plot([a], [b], '*')
text( a+bit, b+bit, 'P_0')

disp('    ')
disp('    iterate      a         b         f(a,b)     g(a,b)')
fprintf('       %d     %0.5f   %0.5f   %0.5f   %0.5f\n', 0, a,b,f0,g0)


text( a+bit, b+bit, 'P_0')
a0 = a; b0 = b;


for n = 1:3

  fx = (feval(f,a+h,b) - feval(f,a-h,b))/(2*h);
  fy = (feval(f,a,b+h) - feval(f,a,b-h))/(2*h);

  gx = ( feval(g,a+h,b) - feval(g,a-h,b))/(2*h);
  gy = ( feval(g,a,b+h) - feval(g,a,b-h))/(2*h);


  yline1 = b -(feval(f,a,b) +fx*(x-a))/fy;
  line1 = plot(x,yline1,'r');
  yline2 = b - (feval(g,a,b) + gx*(x-a))/gy;
  line2 = plot(x,yline2, 'r');

  [a,b] = ginput(1);
  if n==1
     text(a+bit, b+bit, 'P_1')
  elseif n ==2
     text(a+bit, b+bit, 'P_2')
  else
     text(a +bit, b+bit, 'P_3')
  end
  ff = feval(f,a,b); gg = feval(g,a,b);
  fprintf('       %d     %0.5f   %0.5f   %0.5f   %0.5f\n', n, a,b,ff,gg)

  plot([a], [b], '*')
  delete(line1)
  delete(line2)
end
hold off


