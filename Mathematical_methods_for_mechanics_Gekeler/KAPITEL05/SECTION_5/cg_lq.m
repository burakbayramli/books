function [Z,d_omega,errorcode] = cg_lq(F,omega,mue,tol,U,V,W,Parmeter);
% Method of conjugate gradients for continuation contin.m
[p,n]     = size(U);
errorcode = 0; d_omega = 0; r_om = 0;
S = W; Z = zeros(p,n);
R         = back_diff(2,S);
U_S       = [U; S];
GRADTUS   = feval(F,5,mue,U_S,Parmeter);
R         = omega*R + GRADTUS;
r_om      = sum(diag(V*S'))/n;
P         = R;
p_om      = r_om;
pnorm     = sqrt(sum(diag(P*P'))/n);
rr        = sum(diag(R*R'))/n;
rr        = rr + r_om*r_om;
count     = 0;
done      = (pnorm < tol);
while ~done
   count   = count + 1;
   Q       = back_diff(1,P);
   U_P     = [U; P];
   GRADUP  = feval(F,4,mue,U_P,Parmeter);
   Q       = omega*Q + GRADUP;
   Q       = Q + p_om*V;
   qq      = sum(diag(Q*Q'))/n;
   a       = rr/qq;
   Z       = Z + a*P;
   S       = S - a*Q;
   d_omega = d_omega + a*p_om;
   R       = back_diff(2,S);
   U_S     = [U; S];
   GRADTUS = feval(F,5,mue,U_S,Parmeter);
   R       = omega*R + GRADTUS;
   r_om    = sum(diag(V*S'))/n;
   rr_neu  = sum(diag(R*R'))/n;
   rr_neu  = rr_neu + r_om*r_om;
   b       = rr_neu/rr;
   rr      = rr_neu;
   P       = R + b*P;
   p_om    = r_om + b*p_om;
   rnorm   = sqrt(sum(diag(R*R'))/n);
   done    = (rnorm < tol) | (count > p*n);
end;
if count > p*n, errorcode = 2; end
