% Get guass points and weights in a brick master element
% Input: n_xi,n_eta,n_zeta: number of Gauss points in three directions 
% Output: gauss_points: vectors stores the locations of Gauss points
% Output: gauss_weights: the corresponding weights of the Gauss points
function [gauss_points,gauss_weights]=GetHexaGauss(n_xi,n_eta,n_zeta)
ng=n_xi*n_eta*n_zeta;     % total number of Gauss points
gauss_points=zeros(ng,3); % set up empty return variables
gauss_weights=zeros(ng,1);
% if-else block: assign guass points and weights 
% currently only allows 8 point Gauss quadrature,
% can be extended if needed
if ng==8
  pos=sqrt(3.0)/3.0;
  gauss_points(:,:)=pos;
  gauss_points(1,1:3)= -pos;
  gauss_points(2,2:3)= -pos;
  gauss_points(3,3)= -pos;
  gauss_points(4,1)= -pos;
  gauss_points(4,3)= -pos;
  gauss_points(5,1:2)= -pos;
  gauss_points(6,2)= -pos;
  gauss_points(8,1)= -pos;
  gauss_weights(:,:)=1.0;
else
  fprintf('Error calling GetHexaGaussn'); % error message 
  return;
end      