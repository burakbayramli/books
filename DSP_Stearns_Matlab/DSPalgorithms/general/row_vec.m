function v=row_vec(A)
% v=row_vec(A)
%
% Converts any 1, 2, or 3-d array A into row vector v.
% If A is 1xN, v=A. If A is Nx1, v=A.'.
% If A is a matrix, v=A scanned by rows.
% If A has 3 dimensions, pages are scanned sequentially.
% See also: col_vec

[Nr,Nc,Np,N4]=size(A);
if N4>1,
   error('Array has more than 3 dimensions');
else
   for p=1:Np,
      A2=A(:,:,p)';
      v((p-1)*Nr*Nc+1:p*Nr*Nc)=A2(:)';
   end
end
      