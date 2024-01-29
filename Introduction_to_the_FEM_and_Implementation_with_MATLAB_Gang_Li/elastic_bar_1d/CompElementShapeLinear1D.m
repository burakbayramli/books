% Compute the value of the 1-D linear shape function and its first 
% derivative at a set of input points. 
% Input: start_x, end_x: starting and ending points of the element
% Input: x_vector: a list of input positions where the shape function 
%        and its derivative should be evaluated. 
% Output: N, Nx: shape function and its x-derivative are stored in N 
%         and Nx, respectively, in the format shown as follows
%         N=[ N1(x1) N1(x2) N1(x3) ...
%             N2(x1) N2(x2) N2(x3) ...]
%         Nx=[N'1(x1) N'1(x2) N'1(x3) ...
%             N'2(x1) N'2(x2) N'2(x3) ...]
function [N,Nx]=CompElementShapeLinear1D(start_x, end_x, x_vector)
n=size(x_vector,1);                % obtain the size of x_vector
[N, Nx]= deal(zeros(2,n));         % setup empty N and Nx
for i=1:n                          % loop over each point x
  N(1,i)=(x_vector(i) - end_x)/(start_x - end_x);   % N1(x)
  N(2,i)=(x_vector(i) - start_x)/(end_x - start_x); % N2(x)
  Nx(1,i)=1/(start_x - end_x);                      % N'1(x)
  Nx(2,i)=1/(end_x - start_x);                      % N'2(x)
end