function Y = newtonaux1(X,FLAG,Parmeter)
% flag = 1: function
% flag = 2: gradient
% Numerical computation of gradient is too slow

global LEADING_MATRIX_A  RIGHTSIDE_B  BOUNDARY_DATA
global p e t p1 t1
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
U = X(1:N); V = X(N+1:2*N); P = X(2*N+1:2*N+N1);
[CC,DD] = triform2(p,p1,t,t1,U,V);
% -- Leading matrix with convection term is A + AUX1 ----
AUX1 = [CC, zeros(2*N,N1);
        zeros(N1,2*N+N1)];
AUX2 = [DD, zeros(2*N,N1);
        zeros(N1,2*N+N1)];
% -- Gradient of Leading matrix with Triform is A + AUX1 + AUX2
AA = LEADING_MATRIX_A + AUX1; 
AAA = AA + AUX2;
B = RIGHTSIDE_B;
J  = BOUNDARY_DATA(1,:);

bc = 1;
if bc == 1
% DIRICHLET boundary condition ----
J  = BOUNDARY_DATA(1,:);
B = B - AA(:,J)*BOUNDARY_DATA(2,:).'; 
B(J) = BOUNDARY_DATA(2,:).';
AA(J,:) = 0; AA(:,J) = 0;
AAA(J,:) = 0; AAA(:,J) = 0;
AUX = zeros(size(AA,2),1); AUX(BOUNDARY_DATA(1,:)) = 1;
AA  = spdiags(diag(AA)  + AUX,0,AA);
AAA = spdiags(diag(AAA) + AUX,0,AAA);
end

switch FLAG
case 1
   Y = AA*X - B;
  % YY = residuum(X);
  % DIFF = max(max(abs(Y - YY)))
case 2
   Y = AAA;
   check = 0;
   if check == 1
      YY = newtonaux2(X,2,J);
      DIFF = max(max(abs(Y - YY)))
      DIFF = abs(Y - YY);
      n = length(X); BBB = sparse(n,n);
      for I = 1:n
         for K = 1:n
            if DIFF(I,K) > 1E-3, BBB(I,K) = 1; end
         end    
      end   
      spy(BBB)
      pause
   end
end
