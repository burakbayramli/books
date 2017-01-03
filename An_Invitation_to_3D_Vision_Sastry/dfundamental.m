%-------------------------------------------------------------
%  Jana Kosecka 
%  UC Berkeley
%  April 1999
%  8 point algrithm for fundamental matrix estimation
%  p - fisrt image coordinates
%  q - second image coordinates 
%  F - fundamental matrix
%  F = [f1 f4 f7
%       f2 f5 f8
%       f3 f6 f9]
%  epipolar constraint of the form q'*F*p = 0;
%  optional scaling of the image coordinates for better
%  conditioning

% Algorith, from Chapter 6, "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
%
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003
% ---------------------------------------------------------------------

function  F = dfundamental(p, q)

scaling = 1;
NPOINTS = size(p,2);

if scaling
  Kinv(:,:,1) = chol(inv(p*p'/size(p,2)));
  p = Kinv(:,:,1)*p;
  Kinv(:,:,2) = chol(inv(q*q'/size(q,2)));
  q = Kinv(:,:,2)*q;
else
  Kinv(:,:,1)=eye(3,3);
  Kinv(:,:,2)=eye(3,3);
end
A = zeros(NPOINTS, 9);

if NPOINTS < 9
     error('Too few mesurements')
     return;
end

for i = 1:NPOINTS
    A(i,:) = kron(p(:,i),q(:,i))';
  end
r = rank(A);

if r < 8 
  error('Measurement matrix rank defficient');
end;

[U,S,V] = svd(A);

% pick the eigenvector corresponding to the smallest eigenvalue
f = V(:,9);
est = reshape(f,3,3);
f = Kinv(:,:,2)'*est*Kinv(:,:,1);

[U,S,V] = svd(f);
S(3,3) = 0;
F = U*S*V';

F = F/norm(F,'fro');



