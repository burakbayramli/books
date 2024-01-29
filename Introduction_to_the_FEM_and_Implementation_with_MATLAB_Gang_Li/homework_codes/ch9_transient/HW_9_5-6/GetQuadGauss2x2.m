
function [gauss_points, gauss_weights]=GetQuadGauss2x2()
  
  gauss_points=zeros(4,2);
  gauss_weights=zeros(4,1);
  %---first point
  gauss_points(1,1)=1.0/sqrt(3);
  gauss_points(1,2)=1.0/sqrt(3);
  gauss_weights(1)=1.0;
  
  %---second point  
  gauss_points(2,1)=-1.0/sqrt(3);
  gauss_points(2,2)=1.0/sqrt(3);
  gauss_weights(2)=1.0;
  
  %---third point  
  gauss_points(3,1)=-1.0/sqrt(3);
  gauss_points(3,2)=-1.0/sqrt(3);
  gauss_weights(3)=1.0;
 
  %---fourth point  
  gauss_points(4,1)=1.0/sqrt(3);
  gauss_points(4,2)=-1.0/sqrt(3);
  gauss_weights(4)=1.0;
      