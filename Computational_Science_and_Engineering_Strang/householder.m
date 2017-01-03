%2.3  householder.m

function [U,R] = house(A) % Produce R from Householder reflectors saved in U
[m, n] = size(A); U = zeros(m, n);
for k = 1:n
  w = A(k:m,k);        % start with column k of current A, from diagonal down
  w(1) = w(1)-norm(w); % subtract (w,0,...,0) from a = w. New w = a-r
  u = w/norm(w);       % normalize to unit vector u in the kth reflector H_k
  U(k:m,k) = u;        % save u in U to know the H's that produce Q
  A(k:m,k:n) = A(k:m,k:n)-2*u*(u'*A(k:m,k:n)); % multiply current A by H_k
end
R = triu(A(:,1:n)); % square R from nonzeros on and above diagonal of final A
