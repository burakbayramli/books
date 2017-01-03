% MIT 18.335 - Lecture 2 MATLAB Demo 1
% Show contours of vector norms
% Per-Olof Persson, September 10, 2007

[x,y]=meshgrid(1:3,4:6)   % To describe meshgrid
[x,y]=meshgrid(-1.5:.05:1.5,-1.5:.05:1.5);

% 2-norm
z=sqrt(x.^2+y.^2);
contourf(x,y,z,[0,1]),axis equal
pause

% 1-norm
z=abs(x)+abs(y);
contourf(x,y,z,[0,1]),axis equal
pause

% inf-norm
z=max(abs(x),abs(y));
contourf(x,y,z,[0,1]),axis equal
pause

% p-norm
for p=[1,2,4,7.3,100]
  p
  z=(abs(x).^p+abs(y).^p).^(1/p);
  contourf(x,y,z,[0,1]),axis equal
  pause
end
