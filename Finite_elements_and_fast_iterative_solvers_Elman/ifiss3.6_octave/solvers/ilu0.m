function [L,U] = ilu0(A) 
%ILU0  incomplete factorization with no fill in
%   [L,U] = milu0(A)
%   input
%          A            nonsingular sparse matrix A
%   output
%          L            lower triangular factor 
%          U            upper triangular factor
%
% Note: this function was published on the WWW by Yousef Saad
% http://www-users.cs.umn.edu/~saad/
%---------------------------------------------
% function [L,U] = ilu0 (A) 
% ILU factorization of A. Uses ikj variant of GE
%% column version
%---------------------------------------------

%error('Defunct IFISS function: call "ilu" with setup.type='nofill' instead!');

n = size(A,1) ;
z = zeros(n,1) ;
%%
for j=1:n 
    [ii,jj,rr] = find(A(:,j));
    nzr = length(ii); 
    for i =1:nzr
       z(ii(i)) = rr(i); 
    end
    p = 1; 
    k = ii(p) ;
    while (k < j)    
       piv = A(k,j);
       [ik, jk, rk] = find(A(:,k));
       for i= 1:length(ik)
          if ( z(ik(i)) ~=  0.0  & ik(i) > k) 
             z(ik(i)) = z(ik(i)) - piv*rk(i);
         end
       end
%%       A(ii(i):n,j) = A(ii(i):n,j) - piv*A(ii(i):n,k);
%%       for i=p+1:nzr 
%%          A(ii(i),j) = A(ii(i),j) - piv*A(ii(i),k);
%%       A(ii(i),j) = A(ii(i),j) - piv*A(ii(i),k);
%%       end
     p = p+1;
     k = ii(p) ;
     end  %% while
     %% scale 
     for i =1:nzr
          A(ii(i),j) = z(ii(i));
          z(ii(i)) = 0.0;
     end
     A(j+1:n,j) = A(j+1:n,j)/A(j,j);
 end 
 L = tril(A,-1) + spdiags(ones(n,1),0,n,n) ;
 U = triu(A);
 return