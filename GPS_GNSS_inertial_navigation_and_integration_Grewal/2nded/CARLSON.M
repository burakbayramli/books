function [xout,Cout] = carlson(z,R,H,xin,Cin)
%
% Matlab implementation of Neil A. Carlson's ``square root''
% (Cholesky factor) implementation of the Kalman filter
%
% From the diskette included with
% M. S. Grewal, L. R. Weill and A. P. Andrews
% Global Positioning Systems, Inertial Navigation and Integration
% John Wiley & Sons, 2000.
%
% INPUTS:
%  z    measurement (SCALAR)
%  R    variance of measurement error
%  H    measurement sensitivity (row) vector
%  xin  a priori estimate of state vector
%  Cin  upper triangular Cholesky factor of covariance matrix
%       of a priori state estimation uncertainty
% OUTPUTS:
%  xout a posteriori estimate of state vector
%  Cout upper triangular Cholesky factor of covariance matrix
%       of a posteriori state estimation uncertainty
%
C     = Cin;  % Move for in-place (destructive) calculation.
alpha = R;
delta = z;
  for j=1:length(xin),
  delta  = delta - H(j)*xin(j);
  sigma  = 0;
    for i=1:j,
    sigma  = sigma + C(i,j)*H(i);
    end;
  beta   = alpha;
  alpha  = alpha + sigma^2;
  gamma  = sqrt(alpha*beta);
  eta    = beta/gamma;
  zeta   = sigma/gamma;
  w(j)   = 0;
    for i=1:j;
    tau    = C(i,j);
    C(i,j) = eta*C(i,j) - zeta*w(i);
    w(i)   = w(i) + tau*sigma;
    end;
  end;
Cout    = C;
epsilon = delta/alpha;     % Apply scaling to innovations,
xout    = xin + epsilon*w'; % multiply by unscaled Kalman gain;
return;
