%2.3  gramschmidt.m

for j = 1:n                  % Gram-Schmidt orthogonalization
  v = A(:,j);                % v begins as column j of A
  for i = 1:j-1              % columns up to j - 1, already settled in Q
    R(i,j) = Q(:,i)'*A(:,j); % modify A(:,j) to v for more accuracy
    v = v-R(i,j)*Q(:,i);     % subtract the projection (q_i^T a_j)q_i = (q_i^T v)q_i
  end                        % v is now perpendicular to all of q_1,...,q_{j-1}
  R(j,j) = norm(v);
  Q(:,j) = v/R(j,j);         % normalize v to be the next unit vector q_j
end






