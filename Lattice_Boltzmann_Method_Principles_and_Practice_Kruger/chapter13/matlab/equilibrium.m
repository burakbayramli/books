% This code accompanies
%   The Lattice Boltzmann Method: Principles and Practice
%   T. Krüger, H. Kusumaatmaja, A. Kuzmin, O. Shardt, G. Silva, E.M. Viggen
%   ISBN 978-3-319-44649-3 (Electronic) 
%        978-3-319-44647-9 (Print)
%   http://www.springer.com/978-3-319-44647-9
%
% This code is provided under the MIT license. See LICENSE.txt.
%
% Author: Orest Shardt
% 
% compute equilibrium distributions
%

function feq = equilibrium(rho,u,v)
    feq = zeros(size(rho,1),size(rho,2),9);
    
    subexp1 = 1 - 1.5*(u.^2+v.^2);
    
    feq(:,:,1)=(1/9)*rho.*(subexp1 + 3*u+9/2*u.^2);
    feq(:,:,2)=(1/9)*rho.*(subexp1 + 3*v+9/2*v.^2);
    feq(:,:,3)=(1/9)*rho.*(subexp1 - 3*u+9/2*u.^2);
    feq(:,:,4)=(1/9)*rho.*(subexp1 - 3*v+9/2*v.^2);

    feq(:,:,5)=(1/36)*rho.*(subexp1 + 3*( u+v)+9/2*( u+v).^2);
    feq(:,:,6)=(1/36)*rho.*(subexp1 + 3*(-u+v)+9/2*(-u+v).^2);
    feq(:,:,7)=(1/36)*rho.*(subexp1 + 3*(-u-v)+9/2*(-u-v).^2);
    feq(:,:,8)=(1/36)*rho.*(subexp1 + 3*( u-v)+9/2*( u-v).^2);

    feq(:,:,9)=(4/9)*rho.*(subexp1);
