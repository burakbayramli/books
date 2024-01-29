% Get guass points and weights in the 2x2 square master element
% Input: rows, cols: the rows x cols point Gauss quadrature
% Output: gauss_points: vectors stores the locations of Gauss points
% Output: gauss_weights: the corresponding weights of the Gauss points
function [gauss_points, gauss_weights]=GetQuadGauss(rows, cols)
% set up empty return variables
gauss_points=zeros(rows*cols,2);
gauss_weights=zeros(rows*cols,1);
% if-else block: assign guass points and weights for 
% the given input parameters
% currently only allows 1x1, 2x2 and 3x3 point Gauss quadrature
% can be extended if needed
if rows==1 && cols==1               % 1x1 point Gauss quadrature
  x=0;                              % 1-D Gauss points
  w=4;                              % 1-D weights
elseif rows==2 && cols==2           % 2x2 point Gauss quadrature
  x=[-1.0/sqrt(3) 1.0/sqrt(3)];     % 1-D Gauss points
  w=[1 1]'*[1 1];                   % 1-D weights
elseif rows==3 && cols==3           % 3x3 point Gauss quadrature
  x=[-sqrt(3/5) 0 sqrt(3/5)];       % 1-D Gauss points
  w=[5/9 8/9 5/9]'*[5/9 8/9 5/9];   % 1-D weights
else
  fprintf('Error calling GetQuadGauss\n'); % error message 
  return;
end
% for-loop block: set up the 2-D Gauss points and weights
k=1;
for i=1:rows
  for j=1:cols
    gauss_points(k,1:2)=[x(i) x(j)];
    gauss_weights(k)=w(i,j);
    k=k+1;
  end
end