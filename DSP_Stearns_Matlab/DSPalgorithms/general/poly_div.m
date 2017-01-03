function c=poly_div(b,a,N)
% c=poly_div(b,a,N)
%
% c=vector of coefficients, c(n), of C(z), where
%
% C(z)=c(1)+c(2)z^(-1)+...+c(N)z^(-N+1) = B(z) divided by A(z),
%
% where B(z) and A(z) are similar polynomials in z,
% but of arbitrary length.

b=row_vec(b);
a=row_vec(a);
La=length(a);
if length(b)<=La,
   b=[b,zeros(1,La-length(b)+1)];
end
Lb=length(b);
c=zeros(1,N);
for i=1:N,
   c(i)=b(1)/a(1);                  % ith term in quotient.
   b(1:La-1)=b(2:La)-c(i)*a(2:La);  % First part of next dividend.
   b(La:Lb)=[b(La+1:Lb),0];         % Last part of next dividend.
end