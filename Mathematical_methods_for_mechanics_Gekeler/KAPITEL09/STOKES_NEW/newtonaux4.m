function Y = newtonaux4(X,FLAG,Parmeter)
% same as newtonaux1.m but for augmented matrix and 
% without global variables 
% flag = 1: function
% flag = 2: gradient
% Numerical computation of gradient is too slow

%global LEADING_MATRIX_A  RIGHTSIDE_B  BOUNDARY_DATA
load daten8a p e t p1 t1
load daten8c A B 

N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
U = X(1:N); V = X(N+1:2*N); P = X(2*N+1:2*N+N1);

[CC,DD] = triform2(p,p1,t,t1,U,V);
AA = A; AA = sparse(AA);
AA(1:N,1:N) =  AA(1:N,1:N) + CC(1:N,1:N);  % save storage place!!!
AA(1:N,N+1:2*N) =  AA(1:N,N+1:2*N) + CC(1:N,N+1:2*N);  
AA(N+1:2*N,1:N) =  AA(N+1:2*N,1:N) + CC(N+1:2*N,1:N);  
AA(N+1:2*N,N+1:2*N) =  AA(N+1:2*N,N+1:2*N) + CC(N+1:2*N,N+1:2*N);  

AAA = AA;
AAA(1:N,1:N) =  AAA(1:N,1:N) + DD(1:N,1:N);
AAA(N+1:2*N,N+1:2*N) =  AAA(N+1:2*N,N+1:2*N) + DD(N+1:2*N,N+1:2*N);  
clear A p e t p1 t1
switch FLAG
case 1
   Y = AA*X - B;
case 2
   Y = AAA;
end
