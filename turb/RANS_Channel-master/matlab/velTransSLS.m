%**************************************************************************
%       function, transform of van Driest velocity to van Driest extended 
%       velocity
%**************************************************************************
%   dustar = [1+(y/Rets)dRets/dy] duvd
%
% Inputs:
%   uvd     velocity with van Driest transformation
%   r       density
%   mu      molecular viscosity
%   mesh    mesh structure
%
% Output:
%   ustar   velocity with the extended van Driest transformation
%
function [ustar] = velTransSLS(uvd,ReTst,mesh)

    n = size(uvd,1);

    % Calculating correction factor of semi-local velocoty transformation
    dRTSdy = mesh.ddy*ReTst;
    fact = 1 + mesh.y./ReTst.*dRTSdy;

    ustar = zeros(n,1);
    for i=2:n
        ustar(i) = ustar(i-1) + 0.5*(fact(i)+fact(i-1))*(uvd(i)-uvd(i-1));
    end
    
end




