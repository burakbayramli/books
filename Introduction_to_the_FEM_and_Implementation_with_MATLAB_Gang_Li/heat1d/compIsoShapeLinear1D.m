%function to compute the value of the 1-D isoparametric linear 
%shape function and its derivative. Input parameters are the vector 
%containing a list of points [xi] in the master element (between -1 and 1) 
%where the shape function should be evaluated. The values of the 
%shape function and its x-derivative are stored in N and Nx, 
%respectively, in the format shown as follows
% N=[ N1(xi1) N1(xi2) N1(xi3) ...
%     N2(xi1) N2(xi3) N2(xi3) ...]
% Nx=[N'1(xi1) N'1(xi2) N'1(xi3) ...
%     N'2(xi1) N'2(xi3) N'2(xi3) ...]

function [N,Nx]=compIsoShapeLinear1D(xi_vector,N,Nx)

  size_xi_vector=size(xi_vector);
  n=size_xi_vector(1,1);
  for (i=1:n)
    N(1,i)=(1 - xi_vector(i))/2.0;
    N(2,i)=(1 + xi_vector(i))/2.0;
    Nx(1,i)= -0.5;
    Nx(2,i)= 0.5;
  end
