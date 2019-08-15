function test01
% Eigenvalue problem for Laplace operator in unit square 
% with zero boundary conditions everywhere
% --Parameter ---------------------------------
n = 24; % h = 1/(n+1) , n+2 points
clc
% ----------------------------------
e = ones(n,1); I = eye(n);
B = spdiags([-e, 2*e, -e], -1:1, n, n);
A = (kron(B,I) + kron(I,B))*(n+1)^2;
%h = 1/(n+1);
% -- Eigenvalues ----------------------
p = 1;
EW1A = 4*(n+1)^2*( sin(pi*p/(2*(n+1)))  )^2;
EW1 = 2*EW1A
diff_eigenvalue1 = EW1 - 2*pi*pi
p = 2;
EW2A = 4*(n+1)^2*( sin(pi*p/(2*(n+1)))  )^2;
EW2 = EW1A + EW2A
diff_eigenvalue2 = EW2 - 5*pi*pi
p = 7;
EW3A = 4*(n+1)^2*( sin(pi*p/(2*(n+1)))  )^2;
EW3 = EW1A + EW3A
p = 5;
EW3B = 4*(n+1)^2*( sin(pi*p/(2*(n+1)))  )^2;
EW3_2 = 2*EW3B


diff_eigenvalue3 = EW3 - 50*pi*pi
% -- eigenvectors ---------------------
AUX1 = sin(pi*[1:n]/(n+1)).';
AUX2 = sin(2*pi*[1:n]/(n+1)).';
AUX3 = sin(5*pi*[1:n]/(n+1)).';
AUX4 = sin(7*pi*[1:n]/(n+1)).';

EV1 = kron(AUX1,AUX1);
EV2A = kron(AUX1,AUX2); EV2B = kron(AUX2,AUX1);
EV3A = kron(AUX1,AUX4); EV3B = kron(AUX4,AUX1);
EV3C = kron(AUX3,AUX3);
diff_eigenvector1 = norm(A*EV1 - EW1*EV1)
diff_eigenvector2a = norm(A*EV2A - EW2*EV2A)
diff_eigenvector2b = norm(A*EV2B - EW2*EV2B)

diff_eigenvector3a = norm(A*EV3A - EW3*EV3A)
diff_eigenvector3b = norm(A*EV3B - EW3*EV3B)
diff_eigenvector3c = norm(A*EV3C - EW3_2*EV3C)

% -- Grafik ------------------
grafik = 0;
if grafik == 1     
   X = linspace(0,1,n); Y = linspace(0,1,n);
   [U,V] = meshgrid(X,Y);
   clf
   EV1 = reshape(EV1,n,n); 
   W = griddata(X,Y,EV1,U,V,'cubic');
   mesh(U,V,W), pause
   clf
   EV2A = reshape(EV2A,n,n); 
   W = griddata(X,Y,EV2A,U,V,'cubic');
   mesh(U,V,W), pause
   clf
   EV2B = reshape(EV2B,n,n); 
   W = griddata(X,Y,EV2B,U,V,'cubic');
   mesh(U,V,W), pause
   clf
   EV3A = reshape(EV3A,n,n); 
   W = griddata(X,Y,EV3A,U,V,'cubic');
   mesh(U,V,W), pause
   clf
   EV3B = reshape(EV3B,n,n); 
   W = griddata(X,Y,EV3B,U,V,'cubic');
   mesh(U,V,W), pause
   clf
   EV3C = reshape(EV3C,n,n); 
   W = griddata(X,Y,EV3C,U,V,'cubic');
   mesh(U,V,W), pause
end
