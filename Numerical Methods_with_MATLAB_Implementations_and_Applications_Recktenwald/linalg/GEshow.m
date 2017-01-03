function x = GEshow(A,b,ptol)
% GEshow  Show steps in Gauss elimination and back substitution
%         No pivoting is used.
%
% Synopsis:  x = GEshow(A,b)
%            x = GEshow(A,b,ptol)
%
% Input:     A,b  = coefficient matrix and right hand side vector
%            ptol = (optional) tolerance for detection of zero pivot
%                   Default:  ptol = 50*eps
%
% Output:    x = solution vector, if solution exists

if nargin<3, ptol = 50*eps;  end
[m,n] = size(A);
if m~=n,  error('A matrix needs to be square');  end
nb = n+1;   Ab = [A b];    %  Augmented system
fprintf('\nBegin forward elmination with Augmented system:\n');  disp(Ab);

% --- Elimination
for i = 1:n-1               %  loop over pivot rows
  pivot = Ab(i,i);
  if abs(pivot)<ptol, error('zero pivot encountered');  end
  for k = i+1:n             %  k = index of next row to be eliminated
     Ab(k,i:nb) = Ab(k,i:nb) - (Ab(k,i)/pivot)*Ab(i,i:nb); 
  end
  fprintf('\nAfter elimination in column %d with pivot = %f\n',i,pivot);
  disp(Ab);
end

% --- Back substitution
x = zeros(n,1);           %  preallocate memory for and initialize x
x(n) = Ab(n,nb)/Ab(n,n);
for i=n-1:-1:1
  x(i) = (Ab(i,nb) - Ab(i,i+1:n)*x(i+1:n))/Ab(i,i);
end
