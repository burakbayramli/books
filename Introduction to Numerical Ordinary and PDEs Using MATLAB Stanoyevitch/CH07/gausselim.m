function x=gausselim(A,b)
%Inputs:  Square matrix A, and column vector b of same dimension
%Output:  Column vector solution x of linear system Ax = b obtained
%by Gaussian elimination with partial pivoting, provided coefficient 
%matrix A is nonsingular.
[n,n]=size(A);
Ab=[A';b']'; %form augmented matrix for system

for k=1:n
	[biggest, occured] = max(abs(Ab(k:n,k)));
	if biggest == 0
	error('the coefficient matrix is numerically singular')
   end 
	m=k+occured-1;
	Ab=rowswitch(Ab,k, m);
	for j=k+1:n
		Ab=rowcomb(Ab,k,j,-Ab(j,k)/Ab(k,k));
	end
end
% BACK SUBSTITUTION
x=backsubst(Ab(:,1:n),Ab(:,n+1));
