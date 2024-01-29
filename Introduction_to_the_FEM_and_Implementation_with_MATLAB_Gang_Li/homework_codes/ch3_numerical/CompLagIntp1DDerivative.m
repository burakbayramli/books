function [ux]=compLagIntp1DDerivative(data_points, x_vector)
np=size(data_points,1);
n=size(x_vector,1);
dNdx=CompLagBasis1DDerivative(data_points(:,1), x_vector);
ux=zeros(n,1);
for j=1:n
  for i=1:np
    ux(j,1)=ux(j,1)+dNdx(i,j)*data_points(i,2);  
  end
end