function K=commutation(m,n)
% ----------------------------------------------------------
% PURPOSE: given any (m X n) matrix A returns the (mn X mn) K
%          matrix such that: vec(A')=K*vec(A).
% -----------------------------------------------------------
% USAGE: K=commutation(m,n) where m and n are the numbers of 
%         rows and columns of matrix A.
% -----------------------------------------------------------        

% written by
% Marco Aiolfi
% maiolfi@iol.it
% Bocconi University and Banca Intesa, Milan

if nargin == 1
   n=m
end

% a=normrnd(1,1,m,n); this was replaced by LeSage
a = randn(m,n) + ones(n,m);
x=a(:);
puto=a';
y=puto(:);
z=x*y';
K=zeros(m*n);
for i=1:m*n
   for j=1:m*n
      if sqrt(z(i,j))==abs(x(i))
         K(i,j)=1;
      else
         K(i,j)=0;
      end
   end
end
