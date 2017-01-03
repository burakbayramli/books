function  X=gauseid(A,B,X0,kmax)
%This function finds x=A^-1 B by Gauss-Seidal iteration.
if nargin<4, tol=1e-6; kmax=100;
 elseif kmax<1, tol=max(kmax,1e-16); kmax=1000; 
 else tol=1e-6;
end
if nargin<3, X0=zeros(size(B)); end
NA=size(A,1); NB=size(B,2);
X =X0;  
for k=1: kmax
   X(1,:) =(B(1,:)-A(1,2:NA)*X(2:NA,:))/A(1,1);
   for m=2:NA-1
      tmp =B(m,:)-A(m,1:m-1)*X(1:m-1,:)-A(m,m+1:NA)*X(m+1:NA,:);
      X(m,:) =tmp/A(m,m);
   end
   X(NA,:) =(B(NA,:)-A(NA,1:NA-1)*X(1:NA-1,:))/A(NA,NA);
   if nargout==0, X, end %To see the intermediate results
   if norm(X-X0)/(norm(X0)+eps)<tol, break; end
   X0=X;
end