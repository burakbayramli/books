function [x,w] = GegenbauerGQ(lambda,N);
% function [x,w] = GegenbauerGQ(lambda,N);
% Purpose: Compute the N'th order Gauss quadrature points, x, 
%          and weights, w, associated with the Gegenbauer 
%          polynomial, of type lambda > -1/2
if (N==0) x(1)=0; w(1) = 2; return; end;

% Form symmetric matrix from recurrence.
J = zeros(N+1);
h1 = 2*(0:N)+2*lambda-1;
J = diag(2./(h1(1:N)+2).*sqrt((1:N).*((1:N)+2*lambda-1).*...
       ((1:N)+lambda-0.5).*((1:N)+lambda-0.5)./(h1(1:N)+1)./(h1(1:N)+3)),1);

if (2*lambda-1<10*eps) J(1,1)=0.0;end;
if (abs(lambda)<10*eps) J(1,2) = lambda+0.5;end;
J = J + J';

%Compute quadrature by eigenvalue solve
[V,D] = eig(J); x = diag(D);
w = (V(1,:)').^2*2^(2*lambda)*gamma(lambda+1/2)^2/gamma(2*lambda+1);
return;
