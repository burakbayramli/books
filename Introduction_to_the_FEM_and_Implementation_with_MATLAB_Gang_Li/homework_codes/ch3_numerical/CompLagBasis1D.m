% Input: data_points_x: a n x 1 matrix which stores the x-coordinatex 
%        of the data points to be interplated.
% Input: x_vector: a np x 1 array storing the x-coordinates of a set 
%        of points where the Lagrange basis function is evaluated at
% Output: N, a np x n matrix storing N(i,j) which represents Ni(x_j)
function [N]=CompLagBasis1D(data_points_x, x_vector)
np=size(data_points_x,1);        % number of input points
n=size(x_vector,1);              % number of x points N_i(x_j)
N=ones(np,n);                    % N matrix stores result 
for i=1:np                       % loop over i: i-th basis function
  for j=1:n                      % loop over j: j-th output location
    for k=1:np;                  % loop over k: compute the product
      if i~=k                    % skip when x_i = x_k
        N(i,j)=N(i,j)*(x_vector(j)-data_points_x(k,1))/...
              (data_points_x(i,1)-data_points_x(k,1));
      end
    end
  end
end