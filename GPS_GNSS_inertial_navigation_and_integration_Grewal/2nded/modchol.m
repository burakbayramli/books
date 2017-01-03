function [U,D] = modchol(P)
%
% function [U,D] = modchol(P) (APA 2000.05.09)
%
% Computes modified Cholesky factors U, D of
% symmetric positive definite matrix P such that
% U is unit upper triangular, D is diagonal and
%
%              P = U*D*U^T
%
[rows,cols] = size(P);
if rows ~= cols
   error('Non-square input');
else
   n = rows;
   P = (P+P')/2; % take symmetric part
end;
for j=n:-1:1,
   for i=j:-1:1,
      s = P(i,j);
      for k=j+1:n,
         s = s - U(i,k)*U(j,k)*D(k,k);
      end;
      if i==j
         D(j,j) = s;
         U(j,j) = 1;
      else
         U(i,j) = s/D(j,j);
      end;
   end;
end;