function results = AxialDefResults(e, A, coord, dn)
% results = AxialDefResults(e, A, coord, dn)
% e = modulus of elasticity
% A = Area of cross-section
% coord = coordinates at the element ends
% dn = displacements at element ends
% The output variables are axial strain, axial stress,
% and axial force. 

x1=coord(1); x2=coord(2); L=x2-x1;
eps= [-1,1]/L*dn;
sigma = e*eps;
force = sigma*A;
results=[eps, sigma, force];