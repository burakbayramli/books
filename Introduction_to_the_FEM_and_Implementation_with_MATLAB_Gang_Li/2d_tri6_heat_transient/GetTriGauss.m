% Get Gauss points and weights of triangle elements
function [gauss_points, gauss_weights]=GetTriGauss()
% currently only 6 point Gauss quadrature, can be expanded
gauss_points=zeros(6,2);
gauss_weights=zeros(6,1);
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
gauss_weights(1)=0.109951743655322/2.0;
gauss_weights(2)=0.109951743655322/2.0;
gauss_weights(3)=0.109951743655322/2.0;
gauss_weights(4)=0.223381589678011/2.0;
gauss_weights(5)=0.223381589678011/2.0;
gauss_weights(6)=0.223381589678011/2.0;