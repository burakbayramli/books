function [kappa,lambda]=dir_to_lat(b,a)
% [kappa,lambda]=dir_to_lat(b,a)
% Converts transfer function coefficients from direct to
% recursive lattice form.  Direct form H(Z) is defined by:
%
%                     b(1)+b(2)*z^(-1)+...+b(N)*z^(-(N-1))
%              H(Z) = ------------------------------------
%                      1+a(2)*z^(-1)+...+a(N)*z^(-(N-1))
%
% Intputs:  
%    b = B(z) weight vector of length N.
%    a = A(z) weight vector of length N; a(1)=1.
%
% NOTE: length(a) must be >= length(b).
%
% Outputs:
%    kappa, lamda = Lattice weight vectors - see text diagram.

% See also: lat_to_dir, lat_filter, nr_lat_filter, nr_lat_to_dir,
%           nr_dir_to_lat

b=row_vec(b);
a=row_vec(a);
% Extend b with zeros if necessary.
if length(a)>length(b),
    b=[b,zeros(1,length(a)-length(b))];
end
N=length(b);
% Check for errors.
if(length(a)<N),
   error('Length(a) not >= length(b).');
end
if(a(N)>=1),
   error('a(N)>=1 -- system is unstable.')
end
% Initialize p, s, kappa, and lambda.
p=a;
s=b;
kappa=zeros(1,N-1);
lambda=[zeros(1,N-1),b(N)];
% Update kappa, q, p, s, and lambda recursively.
for n=N:-1:2
   kappa(n-1)=p(n);
   q=p(n:-1:1);
   p=(p(1:n)-kappa(n-1)*q)/(1-kappa(n-1)^2);
   s=s(1:n)-lambda(n)*q;
   lambda(n-1)=s(n-1);
end
