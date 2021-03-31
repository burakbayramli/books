%% GaussSeidel.m 

%% Author: Louise Olsen-Kettle (email: l.kettle1@uq.edu.au, @DrOlsen-Kettle)
%% Code to supplement lecture notes by Louise Olsen-Kettle:
%% Numerical solution of Partial Differential Equations (PDEs)
%% Webpage: http://espace.library.uq.edu.au/view/UQ:239427
%% ISBN: 978-1-74272-149-1

% Gauss-Seidel method for solving Ax = b

file1 = fopen('residual.out','w')


A = [5 0 -2; 3 5 1; 0 -3 4]
b = [7 2 -4]'

D = diag(diag(A))
L = tril(A) - D
U = triu(A) - D

% dimension of A(nxn) and b(n)
n = max(size(b))

% initial guess for x0 (k=0)

x_k = zeros(n,1) ;

k=0;

residual = norm(b-A*x_k);
fprintf(file1,'%d %8.6f \n',k, residual);

x_k_1 = zeros(n,1);

while residual > 1.e-3
     % calculate  x_k_1 at next time step (x(i,k+1))
     for i=1:n
         % calculate sum from j = 1, i-1
         % initialise:
         sum1 = 0;

         for j = 1:i-1
             sum1 = sum1 + A(i,j)*x_k_1(j);
         end

         % calculate sum from j = i+1, n
         % initialise:
         sum2 = 0;

         for j = i+1:n 
             sum2 = sum2 + A(i,j)*x_k(j);
         end

         % update x_k_1:
         x_k_1(i) = (b(i) - sum1 - sum2)/A(i,i);

     end
    
     residual = norm(b-A*x_k_1);
     k=k+1;
     fprintf(file1,'%d %8.6f \n',k, residual);
     x_k = x_k_1;

end

x_k

fprintf('x using Gauss-Seidel iteration is: %8.6f \n', x_k)
fprintf('with %f iterations and residual %8.6f \n', k, residual)

% using the direct matrix solver in Matlab:
x = A\b

fprintf('x using Matlab direct matrix solver: %8.6f \n', x)


fclose(file1)

% plot the residual vs number of iterations
load residual.out

k_vec = residual(:,1);
residual_vec = residual(:,2);

figure(1)
plot (k_vec,residual_vec, '-')
xlabel ('no of iterations')
ylabel ('Residual')

title('Residual after each iteration using Gauss-Seidel method')




