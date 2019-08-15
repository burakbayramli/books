function [nodes,weights] = gauss_2(N)
% Test of suboptimal integration rules
% integration interval [0, 1]
% delta = 1, epsilon = 0
syms x
koeff = zeros(N,N);
maple_flag = 2;
switch maple_flag
case 1 % with MAPLE 
   syms x
   q = x^N*(1-x)^(N-1); q = diff(q,N-1); p = q/x; p = simplify(p);
   koeff =  sym2poly(p);
case 2 % without MAPLE   
   koeff = zeros(1,N);
   for m = 1:N
      k = m-1;
      A = factorial(N-1); B = factorial(N+k);
      C = factorial(k); D = factorial(N-1-k);
      koeff(m) = (-1)^k*A*B/(C*C*(k+1)*D);
   end
   koeff = fliplr(koeff);
end
ROOTS = roots(koeff)';
% Matrix for computation of weights -----
MATRIX = zeros(N,N); MATRIX(1,:) = [0,ROOTS];
for m = 2:N-1
   MATRIX(m,:) = 1;
   MATRIX = MATRIX*diag([0,ROOTS]);
end    
MATRIX(N,:) = ones(1,N);
%----------------------------------------
AUX = [2:N+1]; AUX = fliplr(AUX);
Rightside = ones(N,1)./AUX';  
weights = MATRIX\Rightside; weights = weights';
weights(2:N) = weights(2:N)./ROOTS;
beta0   = 1 - sum(weights);
weights = [beta0,weights(2:N)];
nodes   = [0,ROOTS];


