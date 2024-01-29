% Get guass points and weights in a tetrahedral master element
% Input: ng: number of Gauss points
% Output: gauss_points: vectors stores the locations of Gauss points
% Output: gauss_weights: the corresponding weights of the Gauss points
function [gauss_points, gauss_weights]=GetTetraGauss(ng)
gauss_points=zeros(ng,3); % set up empty return variables
gauss_weights=zeros(ng,1);
% if-else block: assign guass points and weights for 
% the given input parameters
% currently only allows 1 point Gauss quadrature,
% can be extended if needed
if ng==1
  gauss_points(:,:)=1.0/4.0;
  gauss_weights(1,1)=1.0/6.0;
else
  fprintf('Error calling GetTetraGauss\n'); % error message 
  return;
end