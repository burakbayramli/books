% Get guass points and weights for the given number of Gauss points
% in the interval of [-1,1]
% Input: n_guass_points: number of Gauss points
% Output: gauss_points: vectors stores the locations of Gauss points
% Output: gauss_weights: the corresponding weights of the Gauss points
function [gauss_points, gauss_weights]=Get1DGauss(n_gauss_points)
% set up empty return variables
gauss_points=zeros(n_gauss_points,1);
gauss_weights=zeros(n_gauss_points,1);
% switch block, assign guass points and weights for 
% the given input parameter   
switch n_gauss_points        
  case 1                                 % 1 Gauss point
    gauss_points=0; 
    gauss_weights=2.0;       
  case 2                                 % 2 Gauss point
    gauss_points=[-1.0/sqrt(3) 1.0/sqrt(3)]';
    gauss_weights=[1 1]';
  case 3                                 % 3 Gauss point
    gauss_points=[-sqrt(3/5) 0 sqrt(3/5)]';
    gauss_weights=[5/9 8/9 5/9]';
  case 4                                 % 4 Gauss point
    gauss_points=[-0.861136 -0.339981 0.339981 0.861136]';
    gauss_weights=[0.34785 0.652145 0.652145 0.34785]';        
  otherwise                               % max 4 Gauss points here
    fprintf('Error calling Get1DGauss\n') % add more if needed
end