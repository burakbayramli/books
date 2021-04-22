function B=rowmult(A,i,c)
% The command rowmult(A,i,c) multiplies
% row i of the matrix A by the scalar c
% and outputs the resulting matrix.
[m,n]=size(A);
if i<1|i>m
   error('Index out of range')
end
B=A;
B(i,:)=c*A(i,:);
