function c=poly_prod(b,a)
% c=poly_prod(b,a,N)
%
% c=vector of coefficients, c(n), of C(z), where
%
% C(z)=c(1)+c(2)z^(-1)+...+c(N)z^(-N+1) = B(z) times A(z),
%
% where B(z) and A(z) are similar polynomials in z,
% but of arbitrary lengths.

b=row_vec(b);
a=row_vec(a);
c=conv(b,a);