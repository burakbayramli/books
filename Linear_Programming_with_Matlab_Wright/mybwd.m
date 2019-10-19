function x = mybwd(U,w)
% syntax: x = mybwd(U,w)
% input: upper triangular matrix U, real vector w
% backward substition to solve Ux = w

n = length(w);
x = zeros(n,1);

for i=n:-1:1
  if i<n
    J=i+1:n;
    w(i) = w(i) - U(i,J)*x(J);
  end
  x(i) = w(i)/U(i,i);
end

return;
