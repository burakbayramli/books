% MIT 18.335 - Lecture 16 MATLAB Demo 1
% The Jacobi algorithm
% Per-Olof Persson, November 5, 2007

format long

% Create symmetric matrix A
n=5;
A0=randn(n,n); A0=A0+A0';

% Loop and apply Jacobi rotations
A=A0
for k=1:10
  for j=1:n-1
    for i=n:-1:j+1
      J=jacrot(A(i,i),A(j,j),A(i,j));
      A([i,j],:)=J'*A([i,j],:);
      A(:,[i,j])=A(:,[i,j])*J;
    end
  end
  A
  pause
end

% Compare with true eigenvalues
eig(A0)

% Again but with larger matrix

n=128;
A0=randn(n,n); A0=A0+A0';

% Loop and apply Jacobi rotations
A=A0;
for k=1:10
  for j=1:n-1
    for i=n:-1:j+1
      J=jacrot(A(i,i),A(j,j),A(i,j));
      A([i,j],:)=J'*A([i,j],:);
      A(:,[i,j])=A(:,[i,j])*J;
    end
  end
  norm(A-diag(diag(A)))
  pause
end

% Compare with true eigenvalues
[sort(diag(A)),eig(A0),sort(diag(A))-eig(A0)]
