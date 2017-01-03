function eigen2(a)
%EIGEN2	Two by two eigenvalues and eigenvectors.
%	EIGEN2(A) prints the eigenvalues
%	and eigenvectors of a 2 by 2 matrix.
%	If A is not diagonalizable, its single
%	eigenvector is printed twice.

d = a(1,1)*a(2,2) - a(1,2)*a(2,1);
t = a(1,1) + a(2,2);
e1 = (t + sqrt(t^2 - 4*d))/2;
e2 = (t - sqrt(t^2 - 4*d))/2;
if a(1,2) ~= 0
   x1 = [a(1,2); e1-a(1,1)];
   x2 = [a(1,2); e2-a(1,1)];
elseif a(2,1) ~= 0
   x1 = [e1-a(2,2); a(2,1)];
   x2 = [e2-a(2,2); a(2,1)];
else
   x1 = [1; 0];
   x2 = [0; 1];
end

disp(' ')
disp('For this matrix, the polynomial whose roots are the eigenvalues is:')
disp(['   e^2 - ' num2str(t) '*e + ' num2str(d) ' = 0'])

disp(' ')
disp('The first eigenvalue and eigenvector are:')
e1
x1

disp(' ')
disp('The second eigenvalue and eigenvector are:')
e2
x2
