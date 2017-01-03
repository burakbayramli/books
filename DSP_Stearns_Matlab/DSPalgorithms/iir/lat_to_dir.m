function [b,a]=lat_to_dir(kappa,lambda)
% [b,a]=lat_to_dir(kappa,lambda)
% Converts transfer function coefficients from lattice to
% direct form.  Direct form H(Z) is defined by:
%
%                     b(1)+b(2)*z^(-1)+...+b(N)*z^(-(N-1))
%              H(Z) = ------------------------------------
%                      1+a(2)*z^(-1)+...+a(N)*z^(-(N-1))
%
% Inputs:
%    kappa, lamda = Lattice weight vectors - see text diagram.
%                   (Length(lambda) must equal length(kappa)+1.)
% Outputs:  
%    b      = B(z) weight vector of length N.
%    a      = A(z) weight vector of length N; a(1)=1.

% Make kappa, lambda row vectors; N= length of lambda.
% See also: dir_to_lat, lat_filter, nr_lat_filter, nr_lat_to_dir,
%           nr_dir_to_lat

kappa=row_vec(kappa);
lambda=row_vec(lambda); 
N=length(lambda);
% Check for error.
if(length(kappa)~=N-1),
   error('lat_to_dir: Length(lambda) not = length(kappa)+1.');
end
% Initialize p(1:N) and q(1:N) and b(1:N) at stage 0.
p=[1,zeros(1,N-1)];
q=[1,zeros(1,N-1)];
b=lambda(1)*q;
% Update p, q, and b at stages 1 thru N-1.
for n=2:N
   p1=p;
   p=p1+kappa(n-1)*[0,q(1:N-1)];
   q=kappa(n-1)*p1+[0,q(1:N-1)];
   b=b+lambda(n)*q;
end
% Set a equal to final value of p(1:N).
a=p;
