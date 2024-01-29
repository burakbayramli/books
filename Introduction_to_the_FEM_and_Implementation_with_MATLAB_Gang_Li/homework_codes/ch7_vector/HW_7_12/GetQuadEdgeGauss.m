% Get guass points and weights on edges of the square master element
% Input: neg: number of Gauss points on each edge
% Output: gauss_points: vectors stores the locations of Gauss points
% Output: gauss_weights: the corresponding weights of the Gauss points
function [gauss_points, gauss_weights]=GetQuadEdgeGauss(neg)
% set up empty return variables
gauss_points=zeros(neg*4,2);
gauss_weights=zeros(neg*4,1);
% switch block, assign 1-D guass points and weights for 
% the given input parameter 
switch neg        
  case 1                                 % 1 Gauss point per edge
    x=0; 
    w=2.0;       
  case 2                                 % 2 Gauss points per edge
    x=[-1.0/sqrt(3) 1.0/sqrt(3)]';
    w=[1 1]';
  case 3                                 % 3 Gauss points per edge
    x=[-sqrt(3/5) 0 sqrt(3/5)]';
    w=[5/9 8/9 5/9]';
  case 4                                 % 4 Gauss points per edge
    x=[-0.861136 -0.339981 0.339981 0.861136]';
    w=[0.34785 0.652145 0.652145 0.34785]';        
  otherwise                               % max 4 Gauss points here
    fprintf('Error calling Get1DGauss\n') % add more if needed
end
% set up the Gauss points and weights
plus_ones=ones(neg,1);                    % neg x 1 vector of 1.0
minus_ones=-1*ones(neg,1);                % neg x 1 vector of -1.0
gauss_points(:,1)=[-x' minus_ones'  x' plus_ones']';
gauss_points(:,2)=[plus_ones' -x' minus_ones'  x']';
gauss_weights(:,1)=[w' w' w' w']';