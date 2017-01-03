function b=nr_lat_to_dir(kappa)
% b=nr_lat_to_dir(kappa)
% Converts transfer function coefficients from nonrecursive
% lattice to direct form.  Direct form H(Z) is defined by:
%
%              H(Z) = b(1)+b(2)*z^(-1)+...+b(N)*z^(-(N-1))
%
% Inputs:  
%    kappa = Nonrecursive lattice weight vector of length N.
% Outputs:
%    b = B(z) weight vector of length N+1 with b(1)=1.
% See also: nr_lat_filter, nr_dir_to_lat, dir_to_lat, lat_to_dir, lat_filter

% kap = row vector; N= length.
kap=row_vec(kappa);
% Initialize p,q=coeff of P(z),Q(z).
N=length(kap)+1;
p=[1,zeros(1,N-1)];
q=p;
% Update p and q. (Indices start at 0 instead of 1.)
for n=2:N
   p1=p;
   p=p1+kap(n-1)*[0,q(1:N-1)];
   q=kap(n-1)*p1+[0,q(1:N-1)];
end
b=p;