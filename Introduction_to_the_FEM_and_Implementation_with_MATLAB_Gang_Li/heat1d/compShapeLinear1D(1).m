%function to compute the value of the 1-D linear shape function and its
%derivative. Input parameters are the start and end points of the element,
%and the vector containing a list of positions where the shape function
%should be evaluated. The values of the shape function and its x-derivative
%are stored in N and Nx, respectively, in the format shown as follows
% N=[ N1(x1) N1(x2) N1(x3) ...
%     N2(x1) N2(x3) N2(x3) ...]
% Nx=[N'1(x1) N'1(x2) N'1(x3) ...
%     N'2(x1) N'2(x3) N'2(x3) ...]

function [N,Nx]=compShapeLinear1D(start_x, end_x, x_vector,N,Nx)

  size_x_vector=size(x_vector);
  n=size_x_vector(1,1);
  for (i=1:n)
    N(1,i)=(x_vector(i) - end_x)/(start_x - end_x);
    N(2,i)=(x_vector(i) - start_x)/(end_x - start_x);
    Nx(1,i)=1/(start_x - end_x);
    Nx(2,i)=1/(end_x - start_x);
  end
