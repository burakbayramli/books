% MIT 18.335 - Lecture 11 MATLAB Demo 1
% LU Factorization without Pivoting
% Per-Olof Persson, October 17, 2007

% Create matrix A
A=[2,1,1,0; 4,3,3,1; 8,7,9,5; 6,7,9,8]

L1=mkL(4,[-2,-4,-3])
L1*A

L2=mkL(4,[-3,-4])
L2*L1*A

L3=mkL(4,-1)
L3*L2*L1*A

U=L3*L2*L1*A

[L1,inv(L1)]
[L2,inv(L2)]
[L3,inv(L3)]

L=inv(L1)*inv(L2)*inv(L3)

L*U
A
