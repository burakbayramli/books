function [ D Dn ] = calc_el_D(Mu,nu,plane_strain)
%
% calculate a 2D linear isotropic elasticity material matrix
%
% Input:
% Mu: shear modulus
% nu: Poisson's ratio
% plane_strain: 1 for plane strain, 0 for plane stress
%
%
% Output:
% returns the 3x3 D matrix normalized by the (3,3) element, which is 
% returned as Dn (scalar)
%
n=size(Mu,1);
m=size(nu,1);
if(n ~= m)
        error('mismatch of second and first dimension in calc_el_D');
end

D = [];
Dn = [];

Dloc = zeros(3,3);
Dloc(1,1) = 1;
Dloc(2,2) = 1;

for i =1 : n
    if(plane_strain)
        tmp = nu(i)/(1.-nu(i));
        Dloc(1,2) = tmp;
        Dloc(2,1) = tmp;
        Dloc(3,3) = (1-2.*nu(i))/(2*(1-nu(i)));
        fac = Mu(i)*(1-nu(i))/((1+nu(i))*(1-2*nu(i)));
    else % plane stress
        Dloc(1,2) = nu(i);
        Dloc(2,1) = nu(i);
        Dloc(3,3) = (1-nu(i))/2;
        fac = Mu(i)/(1-nu(i)^2);
    end
    Dn(i)    = fac * Dloc(3,3);
    D(i,:,:) = fac * Dloc/Dn(i);
end
if i == 1
    D = reshape(D,3,3);
end