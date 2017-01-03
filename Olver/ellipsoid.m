function [K,Q,D,W,l,qf] = ellipsoid(varargin)
%
%  [K,Q,D,W,l,qf] = ellipsoid(...)
%
%  Draws the ellipsoid 
%      x' * K * x = 1
%    corresponding to the positive definite symmetric 3 by 3 matrix K 
%
%  ellipsoid  with no arguments sets K = eye(3) - a sphere
%  ellipsoid(A)    sets K = A if A is symmetric; otherwise K = A' * A
%  ellipsoid(A,n)  same as ellipsoid(A) with n the number of mesh points
%                    default is n = 25
%  ellipsoid(a,b,c)  sets K = diag([a b c])
%  ellipsoid(a,b,c,n)  sets K = diag([a b c]) and n = # mesh points
%
%
%  K - symmetric positive definite matrix used
%  Q - columns are eigenvectors or principal axes of ellipse
%  D - diagonal matrix with eigenvalues, so K = Q' * D * Q
%  l = sqrt(diag(D)) - lengths of axes of ellipse
%  W = Q*D^(-1/2) - linear transformation mapping unit sphere to ellipsoid
%  qf - string giving quadratic form x'*K*x
%
%  See also ELLIPSE, TR3
%

switch length(varargin)
  case 0
     A = eye(3); n = 25;
  case 1 
     A = varargin{1}; n = 25;
  case 2 
     A = varargin{1}; n = varargin{2};
  case 3 
     A = diag([varargin{1},varargin{2},varargin{3}]); n = 25;
  case 4 
     A = diag([varargin{1},varargin{2},varargin{3}]); n = varargin{4};
end

if A == A' 
   K = A;
   else
   K = A' * A;
end

if K(1,1) == 1, q1s = ''; else q1s = [num2str(K(1,1)),' ']; end
if K(2,2) == 1, q2s = ''; else q2s = [num2str(K(2,2)),' ']; end
if K(3,3) == 1, q3s = ''; else q3s = [num2str(K(3,3)),' ']; end

q12 = abs(2*K(1,2)); q12x = num2str(q12);
s12 = sign(K(1,2));
if s12 > 0, q12s = [' + ',q12x,' x y']; 
else if s12 < 0, q12s = [' - ',q12x,' x y'];
else q12s = '';
end 
end 
q13 = abs(2*K(1,3)); q13x = num2str(q13);
s13 = sign(K(1,3));
if s13 > 0, q13s = [' + ',q13x,' x z']; 
else if s13 < 0, q13s = [' - ',q13x,' x z'];
else q13s = '';
end 
end 
q23 = abs(2*K(2,3)); q23x = num2str(q23);
s23 = sign(K(2,3));
if s23 > 0, q23s = [' + ',q23x,' y z']; 
else if s23 < 0, q23s = [' - ',q23x,' y z'];
else q23s = '';
end 
end 

qf = [q1s,'x^2',q12s,' + ',q2s,'y^2',q13s,q23s,' + ',q3s,'z^2'];

disp(' ')
disp('Quadratic form')
disp(' ')
disp(qf)

disp(' ')

disp('K = ')
disp(K)

[Q,D] = eig(K);

disp('Eigenvectors - principal axes')
disp(' ')
disp(Q)
disp('Eigenvalues')
ev = diag(D);
disp(' ')
disp(ev)
disp('Lengths of principal axes')
l = 1./sqrt(ev);
disp(' ')
disp(l)

W = Q * 1/sqrt(D);

df = linspace(0,pi,n); 
dt = [0:2*pi/n:2*pi]; 

x0 = sin(df)' * cos(dt); 
y0 = sin(df)' * sin(dt);
z0 = cos(df)' * ones(size(dt));

[x,y,z] = tr3(W,x0,y0,z0);

surf(x,y,z);

W1 = W(1,:); W2 = W(2,:); W3 = W(3,:); 
N = diag([1 + 1/norm(W(:,1)),1 + 1/norm(W(:,2)),1 + 1/norm(W(:,3))]);
WN = W * N;
WN1 = WN(1,:); WN2 = WN(2,:);  WN3 = WN(3,:);

xa = [W1;WN1];
ya = [W2;WN2];
za = [W3;WN3];
line(xa,ya,za);

xa = - xa;
ya = - ya;
za = - za;
line(xa,ya,za);

axis equal;
shading interp;
colormap(bone);

title(['Ellipsoid:   ',qf,' = 1']);
