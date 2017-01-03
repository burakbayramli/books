function [L,U,P]=lu_dcmp(A)
%This makes LU decomposition of A 
% with the permutation matrix P denoting the row change during factorization  
%The output matrix LU contains L|U in the lower|upper triangular part, 
% but the diagonal elements of L don't have to be shown, since all of them are 1's.
NA=size(A,1);
AP =[A  eye(NA)]; % Augmented matrix
for k=1: NA-1
   %Partial Pivoting at AP(k,k)
   [akx, kx] =max(abs(AP(k:NA,k)));  
   if (akx<eps)  
      error('Singular matrix and No LU decomposition')
   end
   mx =k+kx-1;  
   if kx>1 % Row change if necessary
      tmp_row =AP(k, :);
      AP(k, :) =AP(mx, :);
      AP(mx, :) =tmp_row;
   end
   % LU decomposition
   for m=k+1: NA
      AP(m,k) =AP(m,k)/AP(k,k);
      AP(m,k+1:NA) =AP(m,k+1:NA)-AP(m,k)*AP(k,k+1:NA);
   end
end
P =AP(1:NA, NA+1:NA+NA);
for m=1:NA
   for n=1:NA
      if m==n, L(m,m)= 1.;  U(m,m)=AP(m,m);
       elseif m>n, L(m,n) =AP(m,n); U(m,n) =0.;
       else L(m,n)=0.; U(m,n) =AP(m,n);
      end
   end
end
if nargout==0, disp('L*U=P*A with'); L,U,P, end
if nargout<=1, L=AP(:,1:NA); end
%You can check if P'*L*U=A?

     
     
