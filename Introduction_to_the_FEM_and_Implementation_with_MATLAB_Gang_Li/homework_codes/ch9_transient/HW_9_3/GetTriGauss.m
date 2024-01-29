% Get guass points and weights in the triangular master element
% Input: ng: number of Gauss points
% Output: gauss_points: vectors stores the locations of Gauss points
% Output: gauss_weights: the corresponding weights of the Gauss points
function [gauss_points, gauss_weights]=GetTriGauss(ng)
% set up empty return variables
gauss_points=zeros(ng,2);
gauss_weights=zeros(ng,1);

if ng==1
  gauss_points(1,1)=1/3;
  gauss_points(1,2)=1/3;
  gauss_weights(1)=1/2;
elseif ng==3     
  gauss_points(1,1)=1/6;
  gauss_points(1,2)=1/6;
  gauss_points(2,1)=2/3;
  gauss_points(2,2)=1/6;
  gauss_points(3,1)=1/6;
  gauss_points(3,2)=2/3;
  gauss_weights(1)=1/6;
  gauss_weights(2)=1/6;
  gauss_weights(3)=1/6;
elseif ng==6
  gauss_points(1,1)=0.091576213509661;  
  gauss_points(1,2)=0.091576213509661;
  gauss_points(2,1)=0.816847572980459;  
  gauss_points(2,2)=0.091576213509661;
  gauss_points(3,1)=0.091576213509661;  
  gauss_points(3,2)=0.816847572980459;
  gauss_points(4,1)=0.445948490915965;  
  gauss_points(4,2)=0.10810301816807;
  gauss_points(5,1)=0.445948490915965;  
  gauss_points(5,2)=0.445948490915965;
  gauss_points(6,1)=0.10810301816807;   
  gauss_points(6,2)=0.445948490915965;
  gauss_weights(1)= 0.109951743655322/2.0;
  gauss_weights(2)= 0.109951743655322/2.0;
  gauss_weights(3)= 0.109951743655322/2.0;
  gauss_weights(4)= 0.223381589678011/2.0;
  gauss_weights(5)= 0.223381589678011/2.0;
  gauss_weights(6)= 0.223381589678011/2.0;
elseif ng==7
  gauss_points(1,1)= 0.1012865073235;
  gauss_points(1,2)= 0.1012865073235;
  gauss_points(2,1)= 0.7974269853531;  
  gauss_points(2,2)= 0.1012865073235;
  gauss_points(3,1)= 0.1012865073235;  
  gauss_points(3,2)= 0.7974269853531;  
  gauss_points(4,1)= 0.4701420641051;  
  gauss_points(4,2)= 0.0597158717898;
  gauss_points(5,1)= 0.4701420641051;    
  gauss_points(5,2)= 0.4701420641051;  
  gauss_points(6,1)= 0.0597158717898;   
  gauss_points(6,2)= 0.4701420641051;  
  gauss_points(7,1)= 1/3;   
  gauss_points(7,2)= 1/3;
  gauss_weights(1)=  0.1259391805448;
  gauss_weights(2)=  0.1259391805448;
  gauss_weights(3)=  0.1259391805448;
  gauss_weights(4)=  0.1323941527885;
  gauss_weights(5)=  0.1323941527885;
  gauss_weights(6)=  0.1323941527885;
  gauss_weights(7)=  0.225;
else 
  fprintf('Error calling GetTriGauss\n'); % error message 
  return;
end