function Y = residuum(X)
% calculates function value for Newton's method
global LEADING_MATRIX_A  RIGHTSIDE_B  BOUNDARY_DATA
global p e t p1 t1

N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
U = X(1:N); V = X(N+1:2*N); P = X(2*N+1:2*N+N1);
CC = triform2(p,p1,t,t1,U,V);
% -- Leading matrix with Triform is A + AUX1 ----
AUX1 = [CC,zeros(2*N,N1);
        zeros(N1,2*N+N1)];
AA = LEADING_MATRIX_A + AUX1; AA  = sparse(AA); 
B = RIGHTSIDE_B;
bc = 1;
if bc == 1
% DIRICHLET boundary condition ----
J  = BOUNDARY_DATA(1,:); 
B = B - AA(:,J)*BOUNDARY_DATA(2,:).'; 
B(J) = BOUNDARY_DATA(2,:).';
AA(J,:) = 0; AA(:,J) = 0;
AUX = zeros(size(AA,2),1); AUX(BOUNDARY_DATA(1,:)) = 1;
AA  = spdiags(diag(AA)  + AUX,0,AA);

end
Y = AA*X - B;
