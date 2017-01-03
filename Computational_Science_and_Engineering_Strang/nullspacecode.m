%8.2  nullspacecode.m

[Q,R] = qr(B');      % square Q, triangular R has n - p zero rows
Qr = Q(1:p,:); Qn = Q(p+1:n,:); E = A*Qn;  % split Q into [Qr Qn] 
y = R(1:p,1:p)'\d; ur = Qr*y;  % particular solution ur to Bu = d
z = (E'*E)\(E'*(b-A*ur));      % best un in the nullspace is Qn*z
uopt = ur + Qn*z;         % uopt minimizes ||Au-b||^2 with Bu = d
