function [C] = GegenbauerP(r,lambda,N);
% function [C] = GegenbauerP(r,lambda,N)
% Purpose: Evaluate Gegenbauer polynomial of type lambda > -1/2
%          points r for order N and returns P[1:length(r))]
% Note   : They are normalized to be orthonormal.

% Turn points into row if needed.
xp = r; dims = size(xp);
if (dims(2)==1) xp = xp'; end;

CL = zeros(N+1,length(xp)); 
% Initial values C_0(x) and C_1(x)
gamma0 = sqrt(pi)*gamma(lambda+0.5)/gamma(lambda+1);
CL(1,:) = 1.0/sqrt(gamma0);
if (N==0) C=CL'; return; end;
gamma1 = (lambda+1/2)^2/(2*lambda+2)*gamma0;
CL(2,:) = (2*lambda+1)*xp/(2*sqrt(gamma1));
if (N==1) C=CL(N+1,:)'; return; end;

% Repeat value in recurrence.
aold = 2/(2*lambda+1)*sqrt((lambda+1/2)^2/(2*lambda+2));

% Forward recurrence using the symmetry of the recurrence.
for i=1:N-1
  h1 = 2*i+2*lambda-1;
  anew = 2/(h1+2)*sqrt( (i+1)*(i+2*lambda)*(i+lambda+1/2)^2/(h1+1)/(h1+3));
  CL(i+2,:) = 1/anew*( -aold*CL(i,:) + xp.*CL(i+1,:));
  aold =anew;
end;
C = CL(N+1,:)';
return