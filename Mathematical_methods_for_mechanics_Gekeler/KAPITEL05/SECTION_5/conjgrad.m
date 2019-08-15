function X = conjgrad(p,n,omega,tol,A,U1,U2,C)
% Method of conjugate gradients after Stoer-Bulirsch p. 576.
X  = zeros(p,n);
Q1 = zeros(2,1);
R1 = Q1; S1 = Q1; S  = C;
% -----------------------------------------------------
% following matrix vector multiplication must be
% adapted to problem ----------------------------------
R = omega*back_diff(2,S) + A'*S;
% -----------------------------------------------------
P     = R;
rr    = sum(diag(R*R'))/n;
pnorm = sqrt(rr);
count = 0;
done  = (pnorm < tol) | (count > p*n);
while ~done
   count = count + 1;
   % --------------------------------------------------
   % following matrix vector multiplication must be
   % adapted to problem -------------------------------
   Q     = omega*back_diff(1,P) + A*P;
   Q1(1) = sum(diag(U1*P'))/n;
   Q1(2) = sum(diag(U2*P'))/n;
   % --------------------------------------------------
   qq    = sum(diag(Q*Q'))/n + Q1'*Q1;
   a     = rr/qq;
   X     = X + a*P;
   S     = S - a*Q;
   S1(1) = S1(1) - a*Q1(1);
   S1(2) = S1(2) - a*Q1(2);
   % --------------------------------------------------
   % following matrix vector multiplication must be
   % adapted to problem -------------------------------
   Q     = omega*back_diff(2,S) + A'*S;
   sq    = 1/sqrt(n);
   R     = (U1*S1(1) + U2*S1(2))*sq;
   R     = Q + R;
   % --------------------------------------------------
   rr1   = sum(diag(R*R'))/n;
   b     = rr1/rr;
   rr    = rr1;
   P     = R + b*P;
   pnorm = sum(diag(P*P'))/n;
   pnorm = sqrt(pnorm);
   done  = (pnorm < tol) | (count > p*n);
end
