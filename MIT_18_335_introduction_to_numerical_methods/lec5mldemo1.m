% MIT 18.335 - Lecture 5 MATLAB Demo 1
% Classical vs. Modified Gram-Schmidt
% See also Trefethen/Bau, Lecture 9, Experiment 2
% Per-Olof Persson, September 19, 2007

% Show how to create a random orthogonal matrix
n=5;
[Q,X]=qr(randn(n));
Q
pause

% Show how to make an ill-conditioned R
n=5;
R=diag(2.^(-1:-1:-n))
pause
R=diag(2.^(-1:-1:-n))*triu(ones(n))
pause
R=diag(2.^(-1:-1:-n))*triu(ones(n)+0.1*randn(n))
pause

% Now for the real demonstration
% Create random Q and ill-conditioned R for n=80
% Compute QR factorization with classical and with
% modified GS, compare diagonal elements of computed R's.

n=80;
R=diag(2.^(-1:-1:-n))*triu(ones(n)+0.1*randn(n));
[Q,X]=qr(randn(n));
A=Q*R;

[QC,RC]=clgs(A);
[QM,RM]=mgs(A);

semilogy(1:80,diag(RC),'o',1:80,diag(RM),'x',1:80,diag(R))
grid on
