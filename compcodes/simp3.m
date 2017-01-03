
%             Function simp3
%
%   3D Simpson's rule over a rectangular solid.  The call is
%   simp3(f,corners) when f is given as an inline function, and
%   simp3('f', corners) when f is given in a mfile. 
%       corners = [xmin, xmax, ymin, ymax, zmin, zman] defines the
%   range of integration. 
%       User is asked to enter the number n of sudivisions in the
%   x direction, the number m in the y directions, and p, the
%   number in the z direction.  m,n and p must be even.

 
function out = simp3(f, corners)

xmin =  corners(1); xmax = corners(2); ymin = corners(3);  ymax  = corners(4);
zmin  = corners(5); zmax = corners(6);
disp('  ')
disp('the number of subdivisions m,n and p in each direction must be even')
subdiv = input('enter the number of subdivisions [n m p]  ')

n = subdiv(1); m = subdiv(2); p = subdiv(3);

x = linspace(xmin, xmax, n+1);
y = linspace(ymin, ymax, m+1);
z = linspace(zmin, zmax, p+1);

[X,Y,Z] = meshgrid(x,y,z);

svecx = 2*ones(1,n+1);
svecx(2:2:n) = 4*ones(1,n/2);
svecx(1) = 1; svecx(n+1) = 1;

svecy = 2*ones(1,m+1);
svecy(2:2:m) = 4*ones(1,m/2);
svecy(1) = 1; svecy(m+1) = 1;

svecz = 2*ones(1,p+1);
svecz(2:2:p) = 4*ones(1,p/2);
svecz(1) = 1; svecz(p+1) = 1;

S2 = svecy'*svecx;
S3 = zeros(m+1,n+1,p+1);

for k = 1:p+1
   S3(:,:, k) = svecz(k)*S2;
end

T = S3.*feval(f,X,Y,Z);
V = (xmax - xmin)*(ymax-ymin)*(zmax - zmin);
out = sum(sum(sum(T)))*V/(27*m*n*p);


