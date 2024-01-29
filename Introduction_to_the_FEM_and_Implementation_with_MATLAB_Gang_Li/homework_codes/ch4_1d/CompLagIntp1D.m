% Input: data_points: a n x 2 matrix. The first column stores the 
%        x-coordinates of the data points to be interplated, the
%        second column stores the function values at the data points. 
% Input: x_vector: np x 1 array storing the x-coordinates of a set of 
%        points where the interpolation function is calculated. 
% Output: u, n x 1 array storing the value of the interpolating funtion 
%         at the points specified by x_vector.
function [u]=compLagIntp1D(data_points, x_vector)
np=size(data_points,1);
n=size(x_vector,1);
N=CompLagBasis1D(data_points(:,1), x_vector);
u=zeros(n,1);
for j=1:n
  for i=1:np
    u(j,1)=u(j,1)+N(i,j)*data_points(i,2);  %u(x_j)=sum(N_i(x_j)*u_i)
  end
end