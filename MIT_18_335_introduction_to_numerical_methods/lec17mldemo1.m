% MIT 18.335 - Lecture 17 MATLAB Demo 1
% The Method of Bisection
% Per-Olof Persson, November 14, 2007

format long

% Create symmetric matrix A
n=6;
randn('state',2)
A0=randn(n,n); A0=A0+A0';
A0,pause

% Reduce to tridiagonal
A=hess(A0);
A,pause

% Compute true eigenvalues
l=eig(A0);
l,pause

% Perform Sturm counts
x=[0,1,10,-10,0.35,0.36,0.35591311858441,0.35591311858440];
for i=1:length(x)
  fprintf('Shift=%20.14f, Count=%d\n',x(i),sturmcount(A,x(i)));
  pause
end
