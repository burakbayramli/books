function [xout,Cout] = potter(z,R,H,xin,Cin)
%
% James H. Potter's square root filtering algorithm
% for the observational update of a Cholesky factor
% of the covariance matrix of state estimation uncertainty.
%
% INPUTS:
%  z    (scalar) measurement value
%  R    variance of measurement (white gaussian) noise
%  H    measurement sensitivity matrix
%  xin  a priori estimate of state
%  Cin  Cholesky factor of covariance matrix
%       of a priori estimation uncertainty
% OUTPUTS:
%  xout a posteriori estimate of state
%  Cout Cholesky factor of covariance matrix
%       of a posteriori estimation uncertainty
%
xout  = xin;
Cout  = Cin;
alpha = 0;
delta = z;
n     = length(xin);
  for i=1:n,
  delta = delta - H(i)*xin(i);
  v(i)  = 0;
    for j=1:n,
    v(i) = v(i) + Cout(j,i)*H(j);
    end;
  alpha = alpha + v(i)^2;
  end;
s       = 1/(R+alpha);
epsilon = delta*s;
sigma   = (1 + sqrt(1 - s*alpha))/alpha;
  for i=1:n,
  beta = 0;
    for j=1:n,
    beta = beta + Cout(i,j)*v(j);
    end;
  xout(i) = xout(i) + beta*epsilon;
  tau     = beta*sigma;
    for j=1:n,
    Cout(i,j) = Cout(i,j) - tau*v(j);
    end;
  end;
return;
