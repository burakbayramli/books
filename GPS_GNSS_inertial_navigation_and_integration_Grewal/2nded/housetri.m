function C = housetri(M)
%
% Householder upper triangularization of a matrix
%
% INPUT:  M, a general matrix
% OUTPUT: C, an upper triangular matrix such that
%            CC' = AA'
%
C          = M;
[rows,cols] = size(C);
r          = cols - rows;
for k=rows:-1:1,
   sigma = 0;
   for j=1:r+k,
      sigma = sigma + C(k,j)^2;
   end;
   a     = sqrt(sigma);
   sigma = 0;
   for j=1:r+k,
      if j==r+k
         v(j) = C(k,j) - a;
      else
         v(j) = C(k,j);
      end;
      sigma = sigma + v(j)^2;
   end;
   a = 2/sigma;
   for i=1:k,
      sigma = 0;
      for j=1:r+k,
         sigma = sigma + C(i,j)*v(j);
      end;
      b = a*sigma;
      for j=1:r+k
         C(i,j) = C(i,j) - b*v(j);
      end;
   end;
end;
