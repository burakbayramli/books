function test02
% grafik for eigenvectors of Poisson problems 
% in unit square with zero boundary conditions
% for x = 0 and x = 1 only
n = 24;
n = 6;
clc, format short, format compact
e1 = ones(n,1); e2 = ones(n+2,1);
I1 = eye(n);    I2 = eye(n+2);
B  = spdiags([-e1, 2*e1, -e1], -1:1, n, n);
C  = spdiags([-e2, 2*e2, -e2], -1:1, n+2, n+2);
C(1,1) = 1; C(n+2,n+2) = 1;
A = (kron(B,I2) + kron(I1,C))*(n+1)^2;

CC = full(C)


% -- Eigenvalues ----------------------
p = 1;
EW1A = 4*(n+1)^2*( sin(pi*p/(2*(n+1)))  )^2;
EW1B = 4*(n+1)^2*( cos(pi*p/(2*(n+1)))  )^2;

EW1 = 2*EW1A, 
diff_eigenvalue1 = EW1 - 2*pi*pi

AUX1 = sin(pi*[1:n]/(n+1)).';
AUX5 = [1,cos(pi*[1:n+1]/(n+1))].';

DIFFB = B*(n+1)^2*AUX1 - 0.5*EW1*AUX1;
NORMDIFFB = norm(DIFFB)
DIFFC = C*(n+1)^2*AUX5 - 0.5*EW1*AUX5;
NORMDIFFC = norm(DIFFC)

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

AUX5 = [1,cos(pi*[1:n+1]/(n+1))].';
AUX6 = [1,cos(2*pi*[1:n+1]/(n+1))].';
AUX7 = [1,cos(5*pi*[1:n+1]/(n+1))].';
AUX8 = [1,cos(7*pi*[1:n+1]/(n+1))].';
EV1  = kron(AUX1,AUX5); 
EV2A = kron(AUX1,AUX6); EV2B = kron(AUX2,AUX5);
EV3A = kron(AUX1,AUX8); 
EV3B = kron(AUX4,AUX5); 
EV3C = kron(AUX3,AUX7); 

diff_eigenvector1  = norm(A*EV1 - EW1*EV1)
diff_eigenvector2a = norm(A*EV2A - EW2*EV2A)
diff_eigenvector2b = norm(A*EV2B - EW2*EV2B)
diff_eigenvector3a = norm(A*EV3A - EW3*EV3A)
diff_eigenvector3b = norm(A*EV3B - EW3*EV3B)
diff_eigenvector3c = norm(A*EV3C - EW3_2*EV3C)

grafik = 0;
if grafik == 1   
   % -- mixed boundary conditions --------
   X = linspace(0,1,n+2); Y = linspace(0,1,n+2);
   [U,V] = meshgrid(X,Y);
   clf
   AUX = reshape(EV1,n+2,n); 
   AUX = [zeros(n+2,1),AUX,zeros(n+2,1)];
   W = griddata(X,Y,AUX,U,V,'cubic');
   mesh(U,V,W), xlabel('x'), ylabel('y'), pause
   clf
   AUX = reshape(EV2A,n+2,n); 
   AUX = [zeros(n+2,1),AUX,zeros(n+2,1)];
   W = griddata(X,Y,AUX,U,V,'cubic');
   mesh(U,V,W), xlabel('x'), ylabel('y'), pause
   clf
   AUX = reshape(EV2B,n+2,n);  
   AUX = [zeros(n+2,1),AUX,zeros(n+2,1)];
   W = griddata(X,Y,AUX,U,V,'cubic');
   mesh(U,V,W), xlabel('x'), ylabel('y'), pause
   clf
   AUX = reshape(EV3A,n+2,n); 
   AUX = [zeros(n+2,1),AUX,zeros(n+2,1)];
   W = griddata(X,Y,AUX,U,V,'cubic');
   mesh(U,V,W), xlabel('x'), ylabel('y'), pause
   clf
   AUX = reshape(EV3B,n+2,n); 
   AUX = [zeros(n+2,1),AUX,zeros(n+2,1)];
   W = griddata(X,Y,AUX,U,V,'cubic');
   mesh(U,V,W), xlabel('x'), ylabel('y'), pause
   clf
   AUX = reshape(EV3C,n+2,n); 
   AUX = [zeros(n+2,1),AUX,zeros(n+2,1)];
   W = griddata(X,Y,AUX,U,V,'cubic');
   mesh(U,V,W), xlabel('x'), ylabel('y'),
end


