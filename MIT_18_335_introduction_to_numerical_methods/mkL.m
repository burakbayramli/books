function L=mkL(m,l)
%MKL  Create m-by-m matrix L for vector l.

L=eye(m);
k=m-length(l);
L(k+1:m,k)=l(:);
