function B=rowswitch(A,i,j)
% The command rowswitch(A,i,j) interchanges 
% rows i and j of the matrix A and outputs
% the resulting matrix.
[m,n]=size(A);
if i<1|i>m|j<1|j>m
   error('Index out of range')
end
B=A;
if i==j
   return
end
B(i,:)=A(j,:);
B(j,:)=A(i,:);
