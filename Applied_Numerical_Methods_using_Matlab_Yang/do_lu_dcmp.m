%do_lu_dcmp
clear
%Use LU decomposition to solve Ax=b.
A=hilb(5);
[L,U,P]=lu_dcmp(A); %LU decomposition
x=[1 -2 3 -4 5 -6 7 -8 9 -10]';
b=A*x(1:size(A,1));
flops(0), x_lu=backsubst(U,forsubst(L,P*b)); flps(1)=flops;
flops(0), x_gs=gauss(A,b,0); flps(2)=flops;
flops(0), x_gs2=gauss(A,b); flps(3)=flops;
flops(0), x_bs=A\b; flps(4)=flops;
AI=A^-1;
flops(0), x_iv=AI*b; flps(5)=flops;
disp('     x_lu         x_gs         x_gs2        x_bs        x_iv')
format short e
solutions=[x_lu x_gs x_gs2 x_bs x_iv]
errs=[norm(A*x_lu-b) norm(A*x_gs-b) norm(A*x_gs2-b) norm(A*x_bs-b) norm(A*x_iv-b)]
format short
flps
%Certify that lu_dcmp() works properly
A=[1 2 5;0.2 1.6 7.4;0.5 4 8.5];
[L,U,P]=lu_dcmp(A); %LU decomposition
P.'*L*U-A
[L,U,P]=lu(A)
x=[1 -2 3]'; 
b=A*x
%Cholesky
A=[2 3 4;3 5 6;4 6 9];
U=chol(A)
U'*U-A
%SVD
A=[1 2;2 3;3 5];
[U,S,V]=svd(A)
err=U*S*V'-A