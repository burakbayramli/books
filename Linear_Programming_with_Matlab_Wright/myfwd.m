function w = myfwd(L,b)
% syntax: w = myfwd(L,b)
% input: lower triangular matrix L, real vector b
% forward substition to solve Lw = b

w = b;

for i=1:length(w)
  if i>1
    J=1:i-1;
    w(i) = w(i) - L(i,J)*w(J);
  end
  w(i) = w(i)/L(i,i);
end

return;
