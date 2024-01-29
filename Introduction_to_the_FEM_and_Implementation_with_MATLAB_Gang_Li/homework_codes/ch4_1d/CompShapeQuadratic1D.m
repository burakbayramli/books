% Function to compute the value of the quadratic 1-D isoparametric shape 
% functions and their first derivatives. The input vector  xi_vector 
% contains a list of points [xi] in the master element (between -1 
% and 1) where the shape functions should be evaluated. The values of 
% the shape functions and their x-derivatives are stored in N and Nx, 
% respectively, in the format shown as follows
% N=[ N1(xi1) N1(xi2) N1(xi3) ...
%     N2(xi1) N2(xi2) N2(xi3) ...
%     N3(xi1) N3(xi2) N3(xi3) ...]
% Nx=[N'1(xi1) N'1(xi2) N'1(xi3) ...
%     N'2(xi1) N'2(xi2) N'2(xi3) ...
%     N'3(xi1) N'3(xi2) N'3(xi3) ...]
function [N,Nx]=CompShapeQuadratic1D(N, Nx, xi_vector)
n=size(xi_vector,1);                  % n xi points
for i=1:n
  xi=xi_vector(i);
  N(1,i)=xi*(xi-1)/2.0;      % N1(xi)
  N(2,i)=(1-xi)*(1+xi);      % N2(xi)
  N(3,i)=xi*(xi+1)/2.0;      % N3(xi)
  Nx(1,i)= (2*xi-1)/2.0;     % N'1(xi)
  Nx(2,i)= -2*xi;            % N'2(xi)
  Nx(3,i)= (2*xi+1)/2.0;     % N'3(xi)
end