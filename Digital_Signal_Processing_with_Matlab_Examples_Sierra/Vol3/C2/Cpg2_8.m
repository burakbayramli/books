%Create a tri-banded sparse matrix
%
A=spdiags([2 1 0; 5 4 3; 8 7 6; 11 10 9; 0 13 12],-1:1,5,5);
full(A); %list the matrix in usual format
figure(1)
spy(A,'k'); %visualize the matrix structure (non-zero entries)
%
title('spy diagram');


