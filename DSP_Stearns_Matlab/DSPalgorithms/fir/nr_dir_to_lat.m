function kappa=nr_dir_to_lat(b)
% kappa=nr_dir_to_lat(b)
% Converts transfer function coefficients from nonrecursive
% direct to lattice form.  Direct form H(Z) is defined by:
%
%              H(Z) = b(1)+b(2)*z^(-1)+...+b(N)*z^(-(N-1))
%
% Inputs:  
%    b = B(z) weight vector of length N.
% Outputs:
%    kappa = Nonrecursive lattice weight vector - see text diagram.
%
% See also: nr_lat_filter, nr_lat_to_dir, dir_to_lat, lat_to_dir, lat_filter

% b = row vector; N= length.
b=row_vec(b);
% If b0~=1, adjust b and print warning.
if(b(1)~=1),
   fprintf('CONVERSION WARNING! input weight b(1) is not 1.\n')
   fprintf('Input signal must be subtracted from lattice output.\n')
   b=[1,b];
end
% Initialize p=coeff of P(z), and kappa.
N=length(b);
p=b;
kappa=zeros(1,N-1);
% Find kappa and update q and p. (Indices start at 0 instead of 1.)
for n=N:-1:2
   kappa(n-1)=p(n);
   if(abs(kappa(n-1))==1),
      error('Function aborted because kappa =1.');
   end
   q=p(n:-1:1);
   p=(p(1:n)-kappa(n-1)*q)/(1-kappa(n-1)^2);
end
