function [dNdx]=CompLagBasis1DDerivative(data_points_x, x_vector)
np=size(data_points_x,1);        % number of input points
n=size(x_vector,1);              % number of x points N_i(x_j)
dNdx=zeros(np,n);            
for i=1:np                       % loop over i: i-th basis function
  for j=1:n                      % loop over j: j-th output location
    dN=ones(np,n,np);
    for l=1:np
      if l~=i
        for k=1:np;                  % loop over k: compute the product
          if i~=k                    % skip when x_i = x_k
            if k==l
              dN(i,j,l)=dN(i,j,l)/...
                       (data_points_x(i,1)-data_points_x(k,1));
            else
              dN(i,j,l)=dN(i,j,l)*(x_vector(j)-data_points_x(k,1))/...
                     (data_points_x(i,1)-data_points_x(k,1));
            end
          end   % if i~=k
        end   % if k
        dNdx(i,j)=dNdx(i,j)+ dN(i,j,l); 
      end
    end  % for l
  end  % for j
end  % for i