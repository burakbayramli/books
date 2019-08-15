function [abszissae,weights] = gauss_4(N)
% Test of suboptimal integration rules
% integration interval [0, 1]
% delta = 1, epsilon = 1
syms x
koeff = zeros(N-1,N-1);
maple_flag = 2;
switch maple_flag
case 1 % with MAPLE 
   syms x
   q = x^(N-1)*(1-x)^(N-1); q = diff(q,N-2); p = q/(x*(1-x)); p = simplify(p);
   koeff =  sym2poly(p);
case 2 % without MAPLE   
   koeff = zeros(1,N-1);
   AA = (factorial(N-2))^3*(N-1)^2;
   for L = 1:N-1
      for K = 1:L
         LL = L-1; KK = K-1;
         A = factorial(KK);    B = factorial(N-1-KK);
         C = factorial(LL-KK); D = factorial(N-LL-2);
         AUX = A*A*(KK+1)*B*C*D;
         koeff(L) = koeff(L) + AA/AUX;
      end
      koeff(L) = koeff(L)*(-1)^(L-1);
   end
   koeff = fliplr(koeff);
end
ROOTS = roots(koeff)';
% Matrix for computation of weights ------------
MATRIX = zeros(N,N); MATRIX(1,:) = [0,ROOTS,1];
for m = 2:N-1
   MATRIX(m,:) = 1;
   MATRIX = MATRIX*diag([0,ROOTS,1]);
end    
MATRIX(N,:) = ones(1,N);
% -- Right side -------------------------
AUX1 = [2:N+1]; AUX1 = fliplr(AUX1);
AUX2 = [3:N+2]; AUX2 = fliplr(AUX2);
R = ones(N,1)./AUX1';  % Rechte Seite
R = R./AUX2';
% --  R(i) = int(x*(1-x)*x^i) -----
weights = MATRIX\R; 
weights = weights';
weights(2:N-1) = weights(2:N-1)./(ROOTS.*(1-ROOTS));
AUX       = weights(2:N-1).*ROOTS;
beta1     = 0.5 - sum(AUX);
beta0     = 1 - sum(weights(2:N-1)) - beta1;  
weights   = [beta0,weights(2:N-1),beta1];
abszissae = [0,ROOTS,1];


