function y=nr_lat_filter(kappa,x)
% y=nr_lat_filter(kappa,x)
% Filters vector x using nonrecursive lattice.
%
% Inputs:
%    kappa = weight vector -- see text.
%    x = input signal vector.
% Outputs:  
%    y     = Output signal vector.
%
% See also: nr_dir_to_lat, nr_lat_to_dir, dir_to_lat, lat_to_dir, lat_filter
kap=row_vec(kappa);
N=length(kap)+1; K=length(x);
% Inintialize u and v vectors in lattice. (See text.)
u=zeros(1,N);
v=zeros(1,N);
% Step k from 1 thru K. (Indices start at 1 here, not 0.)
for k=1:K,
   u(1)=x(k);
   for n=2:N,
      u(n)=u(n-1)+kap(n-1)*v(n-1);
   end
   y(k)=u(N);
   v=[u(1),kap.*u(1:N-1)]+[0,v(1:N-1)];
end