function y=lat_filter(kappa,lambda,x)
% y=lat_filter(kappa,lambda,x)
% Filters vector x using lattice filter with weight vectors
% kappa and lambda.
%
% Inputs:
%    kappa, lambda = weight vectors -- see text.
%                    Length(kappa) must equal length(lambda)-1.
%    x = input signal vector.
% Outputs:  
%    y     = Output signal vector.
%
% See also: filter, dir_to_lat, lat_to_dir.
lam=row_vec(lambda);
kap=row_vec(kappa);
N=length(lam);
K=length(x);
if(length(kap)~=N-1)
   error('lat_filter error: length(kappa) not = length(lambda)-1.');
end
% Inintialize u and v vectors in lattice. (See text.)
u=zeros(1,N);
v=zeros(1,N);
% Step k from 1 thru K. (Indices start at 1 here, not 0.)
for k=1:K,
   u(N)=x(k);
   for n=N-1:-1:1,
      u(n)=u(n+1)-kap(n)*v(n);
   end
   v=[u(1),kap.*u(1:N-1)]+[0,v(1:N-1)];
   y(k)=lam*v';
end