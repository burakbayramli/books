% MIT 18.335 - Lecture 11 MATLAB Demo 2
% LU Factorization with Pivoting
% Per-Olof Persson, October 17, 2007

% Create matrix A
A=[2,1,1,0; 4,3,3,1; 8,7,9,5; 6,7,9,8]
U=A;

P1=mkP(4,[1,3])
U=P1*U

L1=mkL(4,-U(2:4,1)/U(1,1))
U=L1*U

P2=mkP(4,[2,4])
U=P2*U

L2=mkL(4,-U(3:4,2)./U(2,2))
U=L2*U

P3=mkP(4,[3,4])
U=P3*U

L3=mkL(4,-U(4,3)./U(3,3))
U=L3*U

L1p=P3*P2*L1/P2/P3
L2p=P3*L2/P3
L3p=L3
L=inv(L1p)*inv(L2p)*inv(L3p)

P=P3*P2*P1

[P*A,L*U]
