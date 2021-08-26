function [x,w] = LegendreGL(m);
% function [x,w] = LegendreGL(m)
% Purpose: Compute the m'th order LGL quadrature points, x, and weights, w 
x = zeros(m+1,1); w = zeros(m+1,1);
if (m==1) x(1)=-1.0; x(2)=1.0; w(1)=1.0; w(2)=1.0; return; end;
if (m==2) 
   x(1)=-1.0; x(2)=0.0; x(3)=1.0; w(1)=1/3; w(2)=4/3; w(3)=1/3; return; 
end;

% Form symmetric matrix from recurrence.
J = zeros(m-1); h1 = 2*(0:m-2)+2;
J = diag(2./(h1(1:m-2)+2).*sqrt((1:m-2).*((1:m-2)+2).*...
       ((1:m-2)+1).*((1:m-2)+1)./(h1(1:m-2)+1)./(h1(1:m-2)+3)),1);
J(1,1)=0.0; J = J + J';

%Compute quadrature by eigenvalue solve
[V,D] = eig(J); x = diag(D); x = [-1, x', 1]';
[P] = LegendreP(x,m); w = (2*m+1)/m/(m+1)./P.^2;
return;