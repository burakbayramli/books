function p=realmmd(A)
%REALMMD Compute true minimum degree permutation for a symmetric matrix.
%   Warning: Very inefficient, use only for small demo problems.

n=size(A,1);
L=zeros(n);
p=1:n;
for i=1:n
  d=sum(~~A,2)-1;
  [foo,ix]=min(d(i:n));
  ix=ix+i-1;
  p([i,ix])=p([ix,i]);
  A([i,ix],:)=A([ix,i],:);
  A(:,[i,ix])=A(:,[ix,i]);
  L(i,i)=sqrt(A(i,i));
  L(i+1:n,i)=A(i+1:n,i)/L(i,i);
  A(i+1:n,i+1:n)=A(i+1:n,i+1:n)-L(i+1:n,i)*L(i+1:n,i)';
  A(i, i:n)=0;
  A(i:n, i)=0;
end
