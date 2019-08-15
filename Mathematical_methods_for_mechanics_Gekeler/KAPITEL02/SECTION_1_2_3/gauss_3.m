function [nodes,weights] = gauss_3(N)
% Test of suboptimal integration rules
% integration interval [0, 1]
% delta = 0, epsilon = 1
syms x
koeff = zeros(N,N);
maple_flag = 2;
switch maple_flag
case 1 % with MAPLE 
   syms x
   q = x^(N-1)*(1-x)^N; q = diff(q,N-1); p = q/(1-x); p = simplify(p);
   koeff =  sym2poly(p);
case 2 % without MAPLE  
   koeff = zeros(1,N); 
   A = (factorial(N-1))^3*N;
   for L = 1:N
      for K = 1:L
         LL = L-1; KK = K-1;
         AUX = (factorial(KK))^2*factorial(N-KK)*factorial(LL-KK);
         AUX = AUX*factorial(N-1-LL);
         koeff(L) = koeff(L) + A/AUX;
      end
      koeff(L) = koeff(L)*(-1)^(L-1);
   end
   koeff = fliplr(koeff);
end
ROOTS = roots(koeff)';
% Matrix for computation of weights -----
MATRIX = zeros(N,N); MATRIX(1,:) = [ROOTS,1];
for M = 2:N-1
   MATRIX(M,:) = 1;
   MATRIX = MATRIX*diag([ROOTS,1]);
end    
MATRIX(N,:) = ones(1,N);
AUX1 = [1:N]; AUX1 = fliplr(AUX1);
AUX2 = [2:N+1]; AUX2 = fliplr(AUX2);

R = ones(N,1)./AUX1';  % Rechte Seite
R = R./AUX2';
% --  R(i) = int((1-x)*x^i) -----
weights = MATRIX\R; weights = weights';
NENNER = ones(1,N-1)- ROOTS;
weights(1:N-1) = weights(1:N-1)./NENNER;
beta1 = 1 - sum(weights);
weights = [weights(1:N-1),beta1];
nodes = [ROOTS,1];
