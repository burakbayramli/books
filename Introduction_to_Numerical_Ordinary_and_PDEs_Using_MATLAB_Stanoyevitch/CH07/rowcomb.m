function B=rowcomb(A,i,j,c)
% Inputs:  A = any matrix,  i, j = row indices,  c = a number
% Output:  B = matrix resulting from A by adding to row j the number
% c times row i.  
[m,n]=size(A);
if i<1|i>m|j<1|j>m
   error('Invalid index')
end
if i==j
   error('Invalid row operation')
end
B=A;
B(j,:)=c*A(i,:)+A(j,:);
