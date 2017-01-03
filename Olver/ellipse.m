function [K,Q,D,W,l,qf] = ellipse(A)
%
%  [K,Q,D,W,l,qf] = ellipse(A)
%
%   Draws the ellipse 
%      x' * K * x = 1
%    corresponding to the positive definite symmetric 2 by 2 matrix 
%    where K = A if A is symmetric; otherwise K = A' * A
%
%  K - symmetric positive definite matrix used
%  Q - orthogonal eigenvectors or principal axes of ellipse
%  D - diagonal matrix with eigenvalues, so K = Q * D * Q'
%  l = 1/sqrt(diag(D)) - lengths of semi-axes of ellipse
%  W = Q*D^(-1/2) - linear transformation mapping unit circle to ellipse
%  qf - string giving quadratic form x'*K*x
%
%  See also ELLIPSOID
%

if size(A) ~= [2 2], error('argument must be 2 by 2 matrix'); end

if A == A' 
   K = A;
   else
   K = A' * A;
end

if K(1,1) == 1, q1s = ''; else q1s = [num2str(K(1,1)),' ']; end
if K(2,2) == 1, q2s = ''; else q2s = [num2str(K(2,2)),' ']; end

q12 = abs(2*K(1,2)); q12s = num2str(q12);
s12 = sign(K(1,2));
if s12 > 0, sg = [' + ',q12s,' x y']; 
else if s12 < 0, sg = [' - ',q12s,' x y'];
else sg = '';
end 
end 
qf = [q1s,'x^2',sg,' + ',q2s,'y^2'];

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

t = linspace(0,2*pi,100);

plot(W(1,1) .* cos(t) + W(1,2) .* sin(t), W(2,1) .* cos(t) + W(2,2) .* sin(t),'m')

W1 = W(1,:); W2 = W(2,:);
N = diag([1 + 1/norm(W(:,1)),1 + 1/norm(W(:,2))]);
WN = W * N;
WN1 = WN(1,:); WN2 = WN(2,:);

xa = [W1;WN1];
ya = [W2;WN2];
line(xa,ya);

xa = - xa;
ya = - ya;
line(xa,ya);

axis equal;

title(['Ellipse:   ',qf,' = 1']);

