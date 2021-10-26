%**************************************************************************
%       function, transform of wall units uplus to van Driest uvd
%**************************************************************************
%   duvd = sqrt(rho/rho_wall) duplus
%
% Inputs:
%   uplus   velocity with transformation using wall values (utau)
%   r       density
%
% Output:
%   uvd     velocity with van Driest transformation
%

function [uvd] = velTransVD(uplus,r)

    n = size(uplus,1);
    uvd = zeros(n,1);
    uvd(1) = 0;
    
    for i=2:n
        uvd(i) = uvd(i-1) + sqrt(0.5*(r(i)+r(i-1))/r(1))*(uplus(i)-uplus(i-1));
    end

end


