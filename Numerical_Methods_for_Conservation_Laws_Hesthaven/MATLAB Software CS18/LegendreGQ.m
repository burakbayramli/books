function [x,w] = LegendreGQ(m);
% function [x,w] = LegendreGQ(m)
% Purpose: Compute the m'th order Legendre Gauss quadrature points, x, 
%          and weights, w
if (m==0) x(1)=0; w(1) = 2; return; end;

% Form symmetric matrix from recurrence.
J = zeros(m+1); h1 = 2*(0:m);
J = diag(2./(h1(1:m)+2).*...
     sqrt((1:m).*((1:m)).*((1:m)).*((1:m))./(h1(1:m)+1)./(h1(1:m)+3)),1);
J(1,1)=0; J = J + J';

%Compute quadrature by eigenvalue solve
[V,D] = eig(J); x = diag(D); w = 2*(V(1,:)').^2;
return