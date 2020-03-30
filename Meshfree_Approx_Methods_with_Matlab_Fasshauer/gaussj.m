function [z,w] = gaussj(n,alf,bet);
%GAUSSJ Nodes and weights for Gauss-Jacobi integration.
%   [X,W] = GAUSSJ(N,ALF,BET) returns nodes and weights for Gauss-Jacobi
%   integration. Z and W are N-vectors such that
%
%              _ +1
%         /
%         |               ALF     BET
%         |     f(x) (1-x)   (1+x)      dx
%         |
%        _/
%          -1
%
%   is approximated by sum(f(Z) .* W).
%
%   Copyright 1997 by Toby Driscoll. Last updated 04/11/97.

%   Uses the Lanczos iteration connection to orthogonal polynomials.
%   Borrows heavily from GAUSSJ out of SCPACK Fortran.

% Calculate coeffs a,b of Lanczos recurrence relation (closed form is
% known).  Break out n=1 specially to avoid possible divide by zero.
apb = alf+bet;
a(1) = (bet-alf)/(apb+2);
b(1) = sqrt(4*(1+alf)*(1+bet) / ((apb+3)*(apb+2)^2));
N = 2:n;
a(N) = (apb)*(bet-alf) ./ ((apb+2*N).*(apb+2*N-2));
N = 2:(n-1);
b(N) = sqrt(4*N.*(N+alf).*(N+bet).*(N+apb) ./ ...
  (((apb+2*N).^2-1).*(apb+2*N).^2));

% Find eigvals/eigvecs of tridiag "Ritz" matrix
if n > 1
  [V,D] = eig(diag(a) + diag(b,1) + diag(b,-1));
else
  V = 1;
  D = a;
end

% Compute normalization (integral of w(x))
c = 2^(apb+1)*gamma(alf+1)*gamma(bet+1)/gamma(apb+2);

% return the values
z = diag(D);
w = c*(V(1,:)').^2;
[z,ind] = sort(z);
w = w(ind);
