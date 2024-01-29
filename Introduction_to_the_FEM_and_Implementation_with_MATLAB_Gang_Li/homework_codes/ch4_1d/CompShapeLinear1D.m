% Function to compute the value of the linear 1-D isoparametric shape 
% functions and their first derivative. The input vector  xi_vector 
% contains a list of points [xi] in the master element (between -1 
% and 1) where the shape function should be evaluated. The values of 
% the shape functions and their x-derivative are stored in N and Nx, 
% respectively, in the format shown as follows
% N=[ N1(xi1) N1(xi2) N1(xi3) ...
%     N2(xi1) N2(xi2) N2(xi3) ...]
% Nx=[N'1(xi1) N'1(xi2) N'1(xi3) ...
%     N'2(xi1) N'2(xi2) N'2(xi3) ...]
function [N,Nx]=CompShapeLinear1D(N, Nx, xi_vector)
n=size(xi_vector,1);                  % n xi points
for i=1:n
  N(1,i)=(1 - xi_vector(i))/2.0;      % N1(xi)
  N(2,i)=(1 + xi_vector(i))/2.0;      % N2(xi)
  Nx(1:2,i)= [-0.5 0.5]';             % Nx(xi)   
end