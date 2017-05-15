% Some mathematics (2)
% inverse and pseudoinverse

%---------------------------------
%2x2 example, nonsingular matrix
A=[1 2;
   3 4];
   
[u,s,v]=svd(A);

si=diag(1./diag(s)); %the inverse of s
Ainv=v*(si)*u'; %inversion using SVD

%check of inversion
A*Ainv

%---------------------------------
%2x2 example, singular matrix
B=[1 2;
   1 2];
   
[U,S,V]=svd(B);

%pseudo-inverse of S
SI=zeros(2,2);
for n=1:2,
   if S(n,n)>0.001, %small threshold
      SI(n,n)=1/S(n,n);
   else
      SI(n,n)=0;
   end;   
end;      

Binv=V*SI*U'; %pseudo-inversion using SVD

%check of pseudo-inversion
B*Binv*B
