function [nodes,weights] = gauss_1(N)
% Test of suboptimal integration rules
% integration interval [0, 1]
% delta = 0, epsilon = 0
maple_flag = 2;
switch maple_flag
case 1 % with MAPLE
   syms x
   p = x^N*(1-x)^N; p = diff(p,N); p = simplify(p);
   koeff =  sym2poly(p);
case 2 % without MAPLE
   koeff = zeros(1,N+1);
   for M = 1:N+1
      K = M-1;
      A = factorial(N+K); B = factorial(N);
      C = factorial(K);   D = factorial(N-K); 
      koeff(M) = (-1)^K*A*B/(C*C*D);
   end
   koeff = fliplr(koeff); 
end   
ROOTS = roots(koeff)';
% Matrix for computation of weights ------------
MATRIX = zeros(N,N); MATRIX(1,:) = ROOTS;
for M = 2:N-1
   MATRIX(M,:) = 1;
   MATRIX = MATRIX*diag(ROOTS);
end    
MATRIX(N,:) = 1;
AUX = [1:N]; AUX = fliplr(AUX);
Rightside = ones(N,1)./AUX';
weights = MATRIX\Rightside;
weights = weights'; nodes = ROOTS;
